package com.automatics.rdkb.tests.ethwan;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;

public class DocsisToEthwanModeTest extends AutomaticsTestBase {

    /**
     * Verify device goes on reboot on switching from DOCSIS mode to EWAN mode.
     * <ol>
     * <li>Verify device is in ETHWAN Mode</li>
     * <li>Disable ETHWAN mode and verify if device goes for reboot</li>
     * <li>Wait till the device is up</li>
     * <li>Verify device operational mode is DOCSIS</li>
     * </li>Verify Triggered CDL Successful with latest firmware version</li>
     * <li>Switch to EWAN mode.(dmcli eRT setv Device.Ethernet.X_RDKCENTRAL-COM_WAN.Enabled bool true)</li>
     * <li>Wait till the device is up</li>
     * <li>Verify reboot reason "WAN_Mode_Change"</li>
     * 
     * <li>Post Condition: Revert device to original build</li>
     * 
     * 
     * @param Dut
     *            {@link device}
     * @author Rucha Sonawane
     * @refactor Athira
     *         </ol>
     */
	
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.UI)
    @TestDetails(testUID = "TC-RDKB-ETHWAN-1001")
    public void testToValidateEthwanModeChange(Dut device) {
    	
    	// Variable Declaration begins
    	String testCaseId = "TC-RDKB-ETHWAN-001";
    	String testStepNumber = "s1";
    	String errorMessage = "";
    	boolean status = false;
    	long startTime = 0L;
    	String currentFirmwareVersion = null;
    	boolean hasBuildChanged = false;
    	String currentImageNameforPostCondition = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);	
    	// Variable Declaration Ends
    	
    	LOGGER.info("#######################################################################################");
    	LOGGER.info("STARTING TEST CASE: TC-RDKB-ETHWAN-1001");
    	LOGGER.info("TEST DESCRIPTION: Verify device goes on reboot on switching from DOCSIS mode to EWAN mode");

    	LOGGER.info("TEST STEPS : ");
    	LOGGER.info("1. Verify device is in ETHWAN Mode");
    	LOGGER.info("2. Disable ETHWAN mode and verify if device goes for reboot");
    	LOGGER.info("3. Wait till the device is up");
    	LOGGER.info("4. Verify device operational mode is DOCSIS");
    	LOGGER.info("5. Verify Triggered CDL Successful with latest firmware version");
    	LOGGER.info("6. Enable ETHWAN mode");
    	LOGGER.info("7. Wait till the device is up ");
    	LOGGER.info("8. Verify reboot reason");
    	LOGGER.info("Post Condition:Revert to original build");
    	LOGGER.info("Post Condition:Revert Device into ETHWAN Mode");
    	LOGGER.info("#######################################################################################");
    	
    	try {
    		/*Verify device is in ETHWAN Mode
    	     * 
    	     */
    	    testStepNumber = "s1";
    	    errorMessage = "Device is not in ethwan mode. skipping test.";
    	    status = false;
    	    LOGGER.info("*******************************************************************************************");
    	    LOGGER.info("STEP 1: DESCRITPION: Verify device is in ETHWAN Mode.");
    	    LOGGER.info("STEP 1: ACTION:  Execute dmcli cmd Device.Ethernet.X_RDKCENTRAL-COM_WAN.Enabled ");
    	    LOGGER.info("STEP 1: EXPECTED: Value should return TRUE.");
    	    LOGGER.info("*******************************************************************************************");
    	    try {
    			status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
    				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHWAN_MODE_ENABLE, BroadBandTestConstants.TRUE,
    				BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
    		    } catch (TestException exception) {
    			errorMessage = errorMessage + " Exception occurred while getting erouter Ipv64 and Ipv4 addresses-> "
    				+ exception.getMessage();
    			LOGGER.error(errorMessage);
    		    }
    	    if (status) {
    			LOGGER.info("STEP 1: ACTUAL : Successfully verified device in ETHWAN Mode.");
    		    } else {
    			LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
    		    }
    		    LOGGER.info("**********************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);    

    		    testStepNumber = "s2";
    		    errorMessage = "Unable to switch device to DOCSIS Mode";
    		    Boolean ethwanDisableStatus = false;
    		    status = false;
    		    LOGGER.info("******************************************************************************");
    		    LOGGER.info("STEP 2: DESCRIPTION: Disable ETHWAN mode");
    		    LOGGER.info("STEP 2: ACTION: Execute Webpa Command to disable ethwan mode.");
    		    LOGGER.info("STEP 2: EXPECTED: Device should reboot automatically");
    		    LOGGER.info("******************************************************************************");
    		    
    		    ethwanDisableStatus = BroadBandWebPaUtils.setWebPAInPolledDuration(device, tapEnv,
    		    		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_SELECTED_OPERATIONAL_MODE, BroadBandTestConstants.CONSTANT_0,
    				    BroadBandTestConstants.STRING_DEVICE_OPERATIONAL_MODE_DOCSIS, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
    				    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
    		  
    		    if(!ethwanDisableStatus) {	
    		    	LOGGER.error("Re-trying with Legacy Ethwan Parameter");
    		    	ethwanDisableStatus = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHWAN_MODE_ENABLE, BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);	    
    		    	ethwanDisableStatus = true;
    		    }
    		    if (ethwanDisableStatus){
    		    	LOGGER.info("Verifying if STB Rebooted.");
    		    	tapEnv.waitTill(AutomaticsConstants.ONE_MINUTE_IN_MILLIS);
    		    	status = CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.CONSTANT_40);
    		    	
    		    }
    		    if(status){
    		    	LOGGER.info("STEP 2: ACTUAL: Successfully enabled ETHWAN Mode on DUT");	
    		    }else{
    		    	LOGGER.info("Device did not go on a reboot.");
    		    	LOGGER.error("STEP 2: ACTUAL: Failed to disable Ethwan Mode "
    		    			+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHWAN_MODE_ENABLE + " webpa params");
    		    }
    		    	 
    		    LOGGER.info("******************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);


    		    // Step 3 : Wait till box is UP.
    		    status = false;
    		    errorMessage = "Failed to access device after WebPa reboot";
    		    testStepNumber = "s3";
    		    LOGGER.info("******************************************************************************");
    		    LOGGER.info("STEP 3: DESCRIPTION: Wait till the device is up.");
    		    LOGGER.info("STEP 3: ACTION: Wait till the device is up.");
    		    LOGGER.info("STEP 3: EXPECTED: Device should be accessible after reboot");
    		    LOGGER.info("******************************************************************************");
    		    tapEnv.waitTill(AutomaticsConstants.FIVE_MINUTES);
    		    status = CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
    		    if (status) {
    			LOGGER.info("STEP 3: ACTUAL: Successfully verified the device came up after reboot");
    		    } else {
    			LOGGER.error("STEP 3: ACTUAL:" + errorMessage);
    		    }
    		    LOGGER.info("******************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

    		    
    		    // wait for 5mins and check device operational mode 	  
    		    testStepNumber = "s4";
    		    errorMessage = "Device operational mode is not DOCSIS";
    		    status = false;
    		    LOGGER.info("*******************************************************************************************");
    		    LOGGER.info("STEP 4: DESCRITPION: Verify device operational mode is DOCSIS.");
    		    LOGGER.info(
    			    "STEP 4: ACTION:  Execute dmcli get for Device.DeviceInfo.X_RDKCENTRAL-COM_EthernetWAN.CurrentOperationalMode ");
    		    LOGGER.info("STEP 4: EXPECTED: Value should return value 'DOCSIS'.");
    		    LOGGER.info("*******************************************************************************************");
    		    startTime = System.currentTimeMillis();
    		    do {
    			try {
    				
    			    status = BroadBandCommonUtils.verifyDeviceInDocsisModeStatusUsingWebPaCommand(tapEnv, device);
    			} catch (TestException exception) {
    			    LOGGER.error("TestException caught with message :" + exception.getMessage() + " : Retrying");
    			}
    		    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS && !status
    			    && CommonMethods.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
    		    if (status) {
    			LOGGER.info("STEP 4: ACTUAL : Device operational mode : " + status);
    		    } else {
    			LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
    		    }
    		    LOGGER.info("**********************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);
    		    

    		    
    		    testStepNumber = "S5";
    		    errorMessage = "Unable to trigger CDL with latest firmware version";
    		    status = false;
    		    String imageNameForCdl = null;
    		    LOGGER.info("**********************************************************************************");
    		    LOGGER.info("STEP 5: DESCRIPTION : Verify Triggered CDL Successful with latest firmware version.");
    		    LOGGER.info("STEP 5: ACTION : Perform image upgrade on the device using TR-181/TFTP DOCSIS SNMP commands.");
    		    LOGGER.info("STEP 5: EXPECTED : CDL Successful with latest firmware version.");
    		    LOGGER.info("**********************************************************************************");
    		    currentFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
    		    
    		    LOGGER.info("LATEST FIRMWARE VERSION: " + imageNameForCdl);
    		    if (CommonMethods.isNull(imageNameForCdl)) {
    			LOGGER.info(
    				" GA image obtained from deployed version service is null. Hence getting the image from property file ");
    			imageNameForCdl=BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,  BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
    			LOGGER.info("Latest Firmware version from property file: " + imageNameForCdl);
    		    }
    		    errorMessage = "Unable to retrieve latest image name for RDKB devices";
    		    LOGGER.info("Latest Image to CDL is: " + imageNameForCdl);
    		    LOGGER.info("Current Firmware Version on device: " + currentFirmwareVersion);
    		    
    		    if (CommonMethods.isNotNull(imageNameForCdl) && !(currentFirmwareVersion.equalsIgnoreCase(imageNameForCdl))){
    		    	status = BroadBandCodeDownloadUtils.triggerHttpCodeDownloadUsingWebpaParameters(device, tapEnv,
    		    			imageNameForCdl);

    		    	hasBuildChanged = status;
    		    }else{
    			LOGGER.info("AS THE CURRENT BUILD & REQUIRED BUILD ARE SAME, SKIPPING THE STEP.");
    			hasBuildChanged = false;
    			status = true;
    			
    		    }
    		    if (status) {
    			LOGGER.info("STEP 5: ACTUAL : Device upgraded successfully with latest firmware version.");
    		    } else {
    			LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
    		    }
    		    LOGGER.info("**********************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
    		    
    		    
    		    testStepNumber = "s6";
    		    errorMessage = "Failed to enable ethwan mode";
    		    status = false;
    		    boolean ethwanEnableStatus = false;
    		    LOGGER.info("******************************************************************************");
    		    LOGGER.info("STEP 6: DESCRIPTION: Enable ETHWAN mode");
    		    LOGGER.info("STEP 6: ACTION: Execute Webpa Command to enable ethwan mode.");
    		    LOGGER.info("STEP 6: EXPECTED: Device should reboot automatically");
    		    LOGGER.info("******************************************************************************");

    		   ethwanEnableStatus = BroadBandWebPaUtils.setWebPAInPolledDuration(device, tapEnv,
    		    		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHWAN_MODE_ENABLE, BroadBandTestConstants.CONSTANT_3,
    				    BroadBandTestConstants.TRUE, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
    				    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
    		    
    		    if(!ethwanEnableStatus){
    		    	LOGGER.error("Re-trying with non-Legacy Ethwan Parameter");
    		    	
    		    	ethwanEnableStatus = BroadBandWebPaUtils.setWebPAInPolledDuration(device, tapEnv,
    			    		BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_SELECTED_OPERATIONAL_MODE, BroadBandTestConstants.CONSTANT_0,
    					    BroadBandTestConstants.STRING_DEVICE_OPERATIONAL_MODE_ETHERNET, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
    					    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
    		    	ethwanEnableStatus = true;
    		    }
    		    if (ethwanEnableStatus) {
    		    	LOGGER.info("Verifying is device went for a reboot.");
    				status = CommonMethods.isSTBRebooted(tapEnv, device, AutomaticsConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.CONSTANT_40);
    						
    		    }	    
    		    if(status){
    		    	LOGGER.info("STEP 6: ACTUAL: Successfully enabled ETHWAN Mode on DUT");	
    		    }else {
    		    LOGGER.info("Device did not go on a reboot.");
    			LOGGER.error("STEP 6: ACTUAL: Failed to enable Ethwan Mode "
    				+ BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_ETHWAN_MODE_ENABLE + " webpa params");
    		    }
    		    LOGGER.info("******************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

    		    
    		    // Step 3 : Wait till box is UP.
    		    status = false;
    		    errorMessage = "Failed to access device after reboot";
    		    testStepNumber = "s7";
    		    LOGGER.info("******************************************************************************");
    		    LOGGER.info("STEP 7: DESCRIPTION: Wait till the device is up.");
    		    LOGGER.info("STEP 7: ACTION: Wait till the device is up.");
    		    LOGGER.info("STEP 7: EXPECTED: Device should be accessible after reboot");
    		    LOGGER.info("******************************************************************************");
    		    tapEnv.waitTill(AutomaticsConstants.FIVE_MINUTES);
    		    status = CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
    		    if (status) {
    			LOGGER.info("STEP 7: ACTUAL: Successfully verified the device came up after reboot");
    		    } else {
    			LOGGER.error("STEP 7: ACTUAL:" + errorMessage);
    		    }
    		    LOGGER.info("******************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

    		    
    		    testStepNumber = "s8";
    		    errorMessage = "Failed to verify last reboot reason.";
    		    status = false;
    		    LOGGER.info("**********************************************************************************");
    		    LOGGER.info("STEP 8: DESCRIPTION: Verify reboot reason.");
    		    LOGGER.info(
    			    "STEP 8: ACTION : EXECUTE WEBPA COMMAND : Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason");
    		    LOGGER.info("STEP 8: EXPECTED: WebPA Parameter should return the value as \"WAN_Mode_Change\".");
    		    LOGGER.info("**********************************************************************************");
    		    try {
    			FirmwareDownloadUtils.verifyLastRebootReasonViaWebpa(tapEnv, device,
    				BroadBandCdlConstants.EXPECTED_LAST_REBOOT_REASON_STATUS_ETHWAN_MODE_CHANGE);
    			status = true;
    		    } catch (Exception exception) {
    			status = false;
    			errorMessage = exception.getMessage();
    		    }
    		    if (status) {
    			LOGGER.info("STEP 8: ACTUAL: Successfully validated the reboot reason as"
    				+ BroadBandCdlConstants.EXPECTED_LAST_REBOOT_REASON_STATUS_ETHWAN_MODE_CHANGE);
    		    } else {
    			LOGGER.error("STEP 8: ACTUAL :" + errorMessage);
    		    }
    		    LOGGER.info("**********************************************************************************");
    		    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
    		
    	}catch (Exception exception) {
    	    errorMessage = exception.getMessage();
    	    LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING ETHWAN MODE CHANGE TEST : " + errorMessage);
    	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
    		    errorMessage, true);
    	}finally {
    	    /**
    	     * Post Condition 1 : Revert the Version back to Initial Firmware Version.
    	     */
    	    if (hasBuildChanged) {
    		BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, hasBuildChanged, false,
    			BroadBandTestConstants.CONSTANT_0, currentImageNameforPostCondition);

    	    }
    	    
    	    /**
    	     * Post Condition 2 : Revert the ETHWAN MOde to Enable Status.
    	     */
    	    
    	    BroadBandPostConditionUtils.executePostConditionToEnableEthwanMode(device, tapEnv, BroadBandTestConstants.CONSTANT_2);
    		

    	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
    	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
    	}
    	
    }
	
}
