/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.snmp;

import java.util.HashMap;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.error.ErrorType;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandSnmpTest extends AutomaticsTestBase {

    /**
     * Verify system description, Modem Configuration Filename,EMTA Address,Cable Interface MAC Address,Serial Number
     * via SNMP and cross validate with WEBPA
     * <ol>
     * <li>STEP 1: Verify Retrieving the sysDescr(.1.3.6.1.2.1.1.1) SNMP MIB command output and check whether it meets
     * the CableLabs specifications</li>
     * <li>STEP 2: Verify Retrieving the Software version via WEBPA Parameter: Device.DeviceInfo.SoftwareVersion and
     * Cross verify with the Value Retrieved from the SNMP value in Step 1.</li>
     * <li>STEP 3: Verify Retrieving the manufacturer name via WEBPA Parameter: Device.DeviceInfo.Manufacturer and Cross
     * verify with the Value Retrieved from the SNMP value in Step 1.</li>
     * <li>STEP 4: Verify Retrieving the Model name via WEBPA Parameter: Device.DeviceInfo.ModelName and Cross verify
     * with the Value Retrieved from the SNMP value in Step 1.</li>
     * <li>STEP 5: Verify Retrieving the Boot Loader Version via WEBPA Parameter:
     * Device.DeviceInfo.X_CISCO_COM_BootloaderVersion and Cross verify with the Value Retrieved from the SNMP value in
     * Step 1.</li>
     * <li>STEP 6: Verify Retrieving the Hardware Version via WEBPA Parameter: Device.DeviceInfo.Hardwareversion and
     * Cross verify with the Value Retrieved from the SNMP value in Step 1.</li>
     * <li>STEP 7: Verify Retrieving the Serial Number via SNMP using: 1.3.6.1.2.1.69.1.1.4.0 MIB and cross verify the
     * value with the response retrieved via WEBPA Parameter:Device.DeviceInfo.SerialNumber.</li>
     * <li>STEP 8: Verify Retrieving the Cable Interface MAC Address via SNMP using:1.3.6.1.2.1.2.2.1.6.2 MIB and cross
     * verify the value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_CableModem.MACAddress.</li>
     * <li>STEP 9: Verify Retrieving the Modem Configuration Filename via SNMP using: 1.3.6.1.2.1.69.1.4.5.0 MIB and
     * cross verify the value with the response retrieved via WEBPA Parameter:
     * Device.X_CISCO_COM_CableModem.BootFileName</li>
     * <li>STEP 10: Verify Retrieving the EMTA Address via SNMP using: 1.3.6.1.2.1.2.2.1.6.16 MIB and cross verify the
     * value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_MTA.MACAddress.</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @author Vignesh
     * @Author Athira
     *
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SNMP_OPERATIONS)
    @TestDetails(testUID = "TC-RDKB-SNMP-1001")
    public void testToVerifySnmpOnDeviceParameters(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SNMP-101";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String snmpSystemDescrOutput = null;
	String softwareVersion = null;
	String manufacturerName = null;
	String manufacturerNameRetrievedViaWebpa = null;
	String modelName = null;
	String bootLoader = null;
	String hardwareVersion = null;
	String serialNumber = null;
	String cableInterfaceMacAddress = null;
	String modemConfigurationFilename = null;
	String emtaAddress = null;

	BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
	// Variable Declaration Ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify system description, Modem Configuration Filename,EMTA Address,Cable Interface MAC Address,Serial Number via SNMP and cross validate with WEBPA");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1. Verify Retrieving the sysDescr(.1.3.6.1.2.1.1.1) SNMP MIB command output and check whether it meets the CableLabs specifications");
	LOGGER.info(
		"2. Verify Retrieving the Software version via WEBPA Parameter: Device.DeviceInfo.SoftwareVersion and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
	LOGGER.info(
		"3. Verify Retrieving the manufacturer name via WEBPA Parameter: Device.DeviceInfo.Manufacturer and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
	LOGGER.info(
		"4. Verify Retrieving the Model name via WEBPA Parameter: Device.DeviceInfo.ModelName  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
	LOGGER.info(
		"5. Verify Retrieving the Boot Loader Version via WEBPA Parameter: Device.DeviceInfo.X_CISCO_COM_BootloaderVersion  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
	LOGGER.info(
		"6. Verify Retrieving the Hardware Version via WEBPA Parameter: Device.DeviceInfo.Hardwareversion  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
	LOGGER.info(
		"7. Verify Retrieving the Serial Number via SNMP using: 1.3.6.1.2.1.69.1.1.4.0 MIB and cross verify the value with the response retrieved via WEBPA Parameter:Device.DeviceInfo.SerialNumber.");
	LOGGER.info(
		"8. Verify Retrieving the Cable Interface MAC Address via SNMP using:1.3.6.1.2.1.2.2.1.6.2 MIB and cross verify the value with the response retrieved via WEBPA Parameter:  Device.X_CISCO_COM_CableModem.MACAddress.");
	LOGGER.info(
		"9. Verify Retrieving the Modem Configuration Filename via SNMP using: 1.3.6.1.2.1.69.1.4.5.0 MIB and cross verify the value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_CableModem.BootFileName");
	LOGGER.info(
		"10. Verify Retrieving the EMTA Address  via SNMP using: 1.3.6.1.2.1.2.2.1.6.16 MIB and cross verify the value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_MTA.MACAddress.");

	LOGGER.info("#######################################################################################");
	try {

	    stepNum = "S1";
	    errorMessage = "SysDesc doesn't match the cablelabs specification with Five Standard Fields SW_REV, MODEL, BOOTR, HW_REV and VENDOR as expected";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify Retrieving the sysDescr(.1.3.6.1.2.1.1.1) SNMP MIB command output and check whether it meets the CableLabs specifications");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute SNMP command to retrive the SysDscr by using SNMP oid 1.3.6.1.2.1.1.1");
	    LOGGER.info(
		    "STEP 1: EXPECTED : Should return SysDescr with only FIVE standard  fields - SW_REV, MODEL, BOOTR, HW_REV and VENDOR. Should satisfy CableLabs specifications");
	    LOGGER.info("**********************************************************************************");

	    /*
	     * Issue SNMP walk command for SysDescr.
	     */
	    snmpSystemDescrOutput = BroadBandSnmpUtils.executeSnmpWalkOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ESTB_SYS_DESCRIPTION.getOid());
	    LOGGER.info("ACTUAL : SNMP Response obtained for Command SysDescr is: " + snmpSystemDescrOutput);
	    HashMap<String, String> systemDescriptor = BroadBandSnmpUtils
		    .parseSystemDescriptorInformationFromSnmpOutput(snmpSystemDescrOutput);
	    status = (systemDescriptor.size() == BroadBandTestConstants.ALLOWED_NUMBER_OF_SYS_DESCRIPTOR_FILED);
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : SysDesc matches the cablelabs specification with Five Standard Fields SW_REV, MODEL, BOOTR, HW_REV and VENDOR as expected");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S2";
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify Retrieving the Software version via WEBPA Parameter: Device.DeviceInfo.SoftwareVersion and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
	    LOGGER.info(
		    "STEP 2: ACTION : \"Execute WEBPA  command retrive to the Software version via WEBPA Parameter: Device.DeviceInfo.SoftwareVersion");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Software Version Retrieved from WEBPA should be same as the Software Version Value Retrived from SNMP in Step 1.");
	    LOGGER.info("**********************************************************************************");
	    softwareVersion = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_SOFTWARE_VERSION);
	    LOGGER.info("Software Version Obtained via SNMP Command is :" + softwareVersion);
	    errorMessage = "Unable to Obtain Software Version or Software Version Obtained via SNMP is null.";
	    if (CommonMethods.isNotNull(softwareVersion)) {
		broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
			tapEnv, BroadBandWebPaConstants.TR69_PARAM_SOFTWARE_VERSION, softwareVersion,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : Software Version Retrieved from WEBPA matches with the Software Version Value Retrived from SNMP in Step 1 as expected");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S3";
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify Retrieving the manufacturer name via WEBPA Parameter: Device.DeviceInfo.Manufacturer and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
	    LOGGER.info(
		    "STEP 3: ACTION : \"Execute WEBPA  command to retrive the manufacturer name via WEBPA Parameter: Device.DeviceInfo.Manufacturer");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Manufacturer Name Retrieved from WEBPA should be same as the Manufacturer name Value Retrived from SNMP in Step 1.");
	    LOGGER.info("**********************************************************************************");
	    manufacturerName = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_VENDOR);
	    LOGGER.info("Manufacturer Name Obtained via SNMP Command is :" + manufacturerName);
	    errorMessage = "Unable to Obtain Manufacturer Name or Manufacturer Name Obtained via SNMP is null.";
	    if (CommonMethods.isNotNull(manufacturerName)) {

		try {

		    manufacturerNameRetrievedViaWebpa = BroadBandCommonUtils.getAutomaticsPropsValueByResolvingPlatform(
			    device, BroadBandTestConstants.MANUFACTURERNAME_VIAWEBPA);

		} catch (Exception e) {
		    manufacturerNameRetrievedViaWebpa = tapEnv.executeWebPaCommand(device,
			    BroadBandWebPaConstants.TR69_PARAM_MANUFACTURER);
		    LOGGER.info("manufacturerName Retrieved Via Webpa as no device specific value found");
		}

		LOGGER.info("Manufacturer Name Obtained via WEBPA Command is :" + manufacturerNameRetrievedViaWebpa);
		errorMessage = "Unable to Obtain Manufacturer Name or Manufacturer Name Obtained via WEBPA is null.";
		if (CommonMethods.isNotNull(manufacturerNameRetrievedViaWebpa)) {
		    errorMessage = "Manufacturer Name Retrieved from WEBPA doesn't match with the Manufacturer Name Value Retrived from SNMP in Step 1 as expected.";
		    status = CommonUtils.patternSearchFromTargetString(manufacturerNameRetrievedViaWebpa,
			    manufacturerName);
		}
	    }
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Manufacturer Name Retrieved from WEBPA matches with the Manufacturer name Value Retrived from SNMP in Step 1 as expected.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S4";
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify Retrieving the Model name via WEBPA Parameter: Device.DeviceInfo.ModelName  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
	    LOGGER.info(
		    "STEP 4: ACTION : \"Execute WEBPA  command to retrive the Model name via WEBPA Parameter: Device.DeviceInfo.ModelName");
	    LOGGER.info(
		    "STEP 4: EXPECTED : Model Name Retrieved from WEBPA should be same as the Model name Value Retrived from SNMP in Step 1.");
	    LOGGER.info("**********************************************************************************");
	    modelName = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_MODEL);
	    LOGGER.info("Model Name Obtained via SNMP Command is :" + modelName);
	    errorMessage = "Unable to Obtain Model Name or Model Name Obtained via SNMP is null.";
	    if (CommonMethods.isNotNull(modelName)) {
		broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
			tapEnv, BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_MODELNAME, modelName,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : Model Name Retrieved from WEBPA matches with the Model Name Value Retrived from SNMP in Step 1 as expected");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S5";
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify Retrieving the Boot Loader Version via WEBPA Parameter: Device.DeviceInfo.X_CISCO_COM_BootloaderVersion  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
	    LOGGER.info(
		    "STEP 5: ACTION : \"Execute WEBPA  command to retrive the Boot Loader Version via WEBPA Parameter: Device.DeviceInfo.X_CISCO_COM_BootloaderVersion");
	    LOGGER.info(
		    "STEP 5: EXPECTED : Boot Loader Version Retrieved from WEBPA should be same as the Boot Loader Version Value Retrived from SNMP in Step 1.");
	    LOGGER.info("**********************************************************************************");
	    bootLoader = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_BOOT_LOADER_VERSION);
	    LOGGER.info("Boot Loader Obtained via SNMP Command is :" + bootLoader);
	    errorMessage = "Unable to Obtain Boot Loader or Boot Loader Obtained via SNMP is null.";
	    if (CommonMethods.isNotNull(bootLoader)) {
		broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
			tapEnv, BroadBandWebPaConstants.TR69_PARAM_DEVICE_INFO_BOOT_LOADER_VERSION, bootLoader,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : Boot Loader Retrieved from WEBPA matches with the Boot Loader Value Retrived from SNMP in Step 1 as expected");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S6";
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify Retrieving the Hardware Version via WEBPA Parameter: Device.DeviceInfo.Hardwareversion  and Cross verify with the Value Retrieved from the SNMP value in Step 1.");
	    LOGGER.info(
		    "STEP 6: ACTION : \"Execute WEBPA  command to retrive Hardware Version via WEBPA Parameter: Device.DeviceInfo.Hardwareversion");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Hardware Version Name Retrieved from WEBPA should be same as the Hardware version name Value Retrived from SNMP in Step 1.");
	    LOGGER.info("**********************************************************************************");
	    hardwareVersion = systemDescriptor.get(BroadBandTestConstants.KEY_SYS_DESCR_HARDWARE_VERSION);
	    LOGGER.info("Hardware Version Obtained via SNMP Command is :" + hardwareVersion);
	    errorMessage = "Unable to Obtain Hardware Version or Hardware Version Obtained via SNMP is null.";
	    if (CommonMethods.isNotNull(hardwareVersion)) {
		broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
			tapEnv, BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_HARDWARE_VERSION, hardwareVersion,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Hardware Version Retrieved from WEBPA matches with the Hardware Version Value Retrived from SNMP in Step 1 as expected");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S7";
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Verify Retrieving the Serial Number via SNMP using: 1.3.6.1.2.1.69.1.1.4.0 MIB and cross verify the value with the response retrieved via WEBPA Parameter:Device.DeviceInfo.SerialNumber.");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute SNMP command to retrive to retrieve the Serial Number (and Cross verify via WEBPA) by using SNMP oid 1.3.6.1.2.1.1.1");
	    LOGGER.info(
		    "STEP 7: EXPECTED : Serial Number Retrieved from SNMP should be same as the Serial Number Value Retrived via WEBPA.");
	    LOGGER.info("**********************************************************************************");

	    serialNumber = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_SERIAL_NUMBER.getOid(), BroadBandSnmpMib.ECM_SERIAL_NUMBER.getTableIndex());
	    LOGGER.info("Serial Number Obtained via SNMP Command is :" + serialNumber);
	    errorMessage = "Unable to Obtain Serial Number or Serial Number Obtained via SNMP is null.";
	    if (CommonMethods.isNotNull(serialNumber)) {
		broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
			tapEnv, BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_SERIAL_NUMBER, serialNumber,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 7: ACTUAL : Serial Number Retrieved from SNMP matches with the Serial Number Value Retrived via WEBPA as expected.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S8";
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify Retrieving the Cable Interface MAC Address via SNMP using:1.3.6.1.2.1.2.2.1.6.2 MIB and cross verify the value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_CableModem.MACAddress");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute the SNMP Get Command to retrieve the Serial Number (and Cross verify via WEBPA) by using SNMP oid 1.3.6.1.2.1.2.2.1.6.2");
	    LOGGER.info(
		    "STEP 8: EXPECTED :  Cable Interface MAC Address Retrieved from SNMP should be same as the Cable Interface MAC Address Value Retrived via WEBPA.");
	    LOGGER.info("**********************************************************************************");

	    cableInterfaceMacAddress = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_CABLE_INTERFACE_MAC_ADDRESS.getOid(),
		    BroadBandSnmpMib.ECM_CABLE_INTERFACE_MAC_ADDRESS.getTableIndex());
	    LOGGER.info("Cable Interface Mac Address Obtained via SNMP Command is :" + cableInterfaceMacAddress);
	    errorMessage = "Unable to Obtain Cable Interface Mac Address or Cable Interface Mac Address Obtained via SNMP is null.";
	    if (CommonMethods.isNotNull(cableInterfaceMacAddress)) {
		broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
			tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_CM_MAC, cableInterfaceMacAddress,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			BroadBandTestConstants.BOOLEAN_VALUE_TRUE);
		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL : Cable Interface MAC Address Retrieved from SNMP matches with the Cable Interface MAC Address Value Retrived via WEBPA as expected.");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S9";
	    errorMessage = null;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify Retrieving the Modem Configuration Filename via SNMP using: 1.3.6.1.2.1.69.1.4.5.0 MIB and cross verify the value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_CableModem.BootFileName");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the SNMP Get Command to retrieve the Serial Number (and Cross verify via WEBPA) by using SNMP oid 1.3.6.1.2.1.69.1.4.5.0");
	    LOGGER.info(
		    "STEP 9: EXPECTED : Modem Configuration Filename Retrieved from SNMP should be same as the Modem Configuration Filename Value Retrived via WEBPA.");
	    LOGGER.info("**********************************************************************************");

	    modemConfigurationFilename = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_MODEM_CONFIGURATION_FILENAME.getOid(),
		    BroadBandSnmpMib.ECM_MODEM_CONFIGURATION_FILENAME.getTableIndex());
	    LOGGER.info("Modem Configuration Filename Obtained via SNMP Command is :" + modemConfigurationFilename);
	    errorMessage = "Unable to Obtain Modem Configuration Filename or Modem Configuration Filename Obtained via SNMP is null.";
	    if (CommonMethods.isNotNull(modemConfigurationFilename)) {
		broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
			tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_BOOT_FILENAME, modemConfigurationFilename,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : Modem Configuration Filename Retrieved from SNMP matches with the Modem Configuration Filename Value Retrived via WEBPA as expected.");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S10";
	    errorMessage = "null";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Verify Retrieving the EMTA Address  via SNMP using: 1.3.6.1.2.1.2.2.1.6.16 MIB and cross verify the value with the response retrieved via WEBPA Parameter: Device.X_CISCO_COM_MTA.MACAddress.");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute the SNMP Get Command to retrieve the Serial Number (and Cross verify via WEBPA) by using SNMP OID 1.3.6.1.2.1.2.2.1.6.16");
	    LOGGER.info(
		    "STEP 10: EXPECTED : EMTA Address  Retrieved from SNMP should be same as the EMTA Address  Value Retrived via WEBPA.");
	    LOGGER.info("**********************************************************************************");

	    emtaAddress = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.EMC_EMTA_ADDRESS.getOid(), BroadBandSnmpMib.EMC_EMTA_ADDRESS.getTableIndex());
	    LOGGER.info("EMTA Address Obtained via SNMP Command is :" + emtaAddress);
	    errorMessage = "Unable to Obtain EMTA Address or EMTA Address Filename Obtained via SNMP is null.";
	    if (CommonMethods.isNotNull(emtaAddress)) {
		broadBandResultObject = BroadBandWebPaUtils.getWebpaValueAndVerifySnmpValueInPolledDuration(device,
			tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_MTA_MAC, emtaAddress,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			BroadBandTestConstants.BOOLEAN_VALUE_TRUE);
		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();
	    }
	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL : MTA Address Retrieved from SNMP matches with the EMTA Address Value Retrived via WEBPA as expected.");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}

	LOGGER.info(" ENDING TEST CASE: TC-RDKB-SNMP-1001");
    }

    /**
     * Test to Verify the Manufacturer serial number using
     * DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) SNMP MIB.
     * 
     * @param device
     *            The device to be used.
     * 
     * @author Selvaraj Mariyappan
     * @Refactor Athira
     * 
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.SNMP_OPERATIONS })
    @TestDetails(testUID = "TC-RDKB-SNMP-1002")

    public void testSnmpGetOnDocsisCableDeviceSerialNumber(Dut device) {
	String testCaseId = "TC-RDKB-SNMP-002";
	String stepNumber = "s1";
	boolean status = false;
	String message = null;
	String snmpDocsisDeviceSerialNumber = null;
	String manufacturerSerialNumber = null;

	/**
	 * Step 1 : Retrieve the Serial number from the response of DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber command
	 * and verify with the actual serial number of the device using device object
	 */

	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP 1:DESCRIPTION: Retrieve the Serial number from the response of DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber command and verify with the actual serial number of the device using device object. If device object not configured webpa param will be used.");
	LOGGER.info(
		"STEP 1:ACTION: Execute snmp command DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber and cross check the value with the value retrived from CATS/ webpa");
	LOGGER.info("EXPECTED: Serial number should be same for snmp and values retrieved using CATS/webpa");
	LOGGER.info("**********************************************************************************");
	try {

	    // Retrieve the serial number from the response
	    snmpDocsisDeviceSerialNumber = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
		    BroadBandSnmpMib.ECM_DOCS_CABLE_DEVICE_MIB_DOCS_DEV_SERIAL_NUMBER.getOid());
	    LOGGER.info("SNMP command output for DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) : "
		    + snmpDocsisDeviceSerialNumber);
	    manufacturerSerialNumber = device.getSerialNumber();
	    LOGGER.info("manufacturerSerialNumber obtained from CATS " + manufacturerSerialNumber);
	    message = " EXPECTED: Manufacturer Serial Number  : " + manufacturerSerialNumber;

	    if (CommonMethods.isNotNull(snmpDocsisDeviceSerialNumber)
		    && CommonMethods.isNotNull(manufacturerSerialNumber)) {
		// Verify with the actual serial number of the device
		status = (snmpDocsisDeviceSerialNumber.trim()).equalsIgnoreCase(manufacturerSerialNumber.trim());
		if (!status) {
		    message = "Seems like DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) providing wrong serial number . ACTUAL : Manufacture Serial number :  "
			    + snmpDocsisDeviceSerialNumber + message;
		    LOGGER.error(message);
		    status = (snmpDocsisDeviceSerialNumber.trim())
			    .equalsIgnoreCase(BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
				    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_SERIAL_NUMBER).trim());
		    LOGGER.error(
			    "Since device object validation failed, Status of serial number validation done via webapa: "
				    + status);
		}
	    } else {
		status = false;
		message = "Unable to retrieve manufacturer serial number using DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) SNMP MIB";
		LOGGER.error(message);
	    }
	    if (status) {
		LOGGER.info("STEP 1 : ACTUAL : Successfully verified serial number");
	    } else {
		LOGGER.error("STEP 1 : ACTUAL : " + message);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + message, true);
	} catch (Exception exception) {
	    status = false;
	    message = "Unable to retrieve DOCS-CABLE-DEVICE-MIB::docsDevSerialNumber(1.3.6.1.2.1.69.1.1.4.0) details using SNMP MIB"
		    + exception.getMessage();
	    LOGGER.error(message);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, message, true);
	}
    }

    /**
     * Test to Verify the WAN MAC Address using IF-MIB::ifPhysAddress.1 (.1.3.6.1.2.1.2.2.1.6.1) SNMP MIB.
     * 
     * 
     * <ol>
     * <li>Step 1 : Retrieve the Mac Address from the response of IF-MIB::ifPhysAddress.1 command and verify with the
     * actual MAC Address of the device using device object</li>
     * </ol>
     * 
     * @author Selvaraj Mariyappan
     * @Refactor Athira
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.SNMP_OPERATIONS })
    @TestDetails(testUID = "TC-RDKB-SNMP-1003")
    public void testSnmpGetOnWanMacAddress(Dut device) {

	String testCaseId = "TC-RDKB-SNMP-003";
	String stepNumber = "s1";
	boolean status = false;
	String errorMessage = null;
	String snmpWanMacAddress = null;
	String wanMacAddress = null;
	String ifTableIndex = null;
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1003");
	LOGGER.info(
		"TEST DESCRIPTION:Verify the WAN MAC Address using IF-MIB::ifPhysAddress.1 (.1.3.6.1.2.1.2.2.1.6) ");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1:Verify the Mac Address from the response of IF-MIB::ifPhysAddress.1(for fibre device getting the erouter0 mib index) command and verify with the actual MAC Address of the device using device object");
	LOGGER.info("#######################################################################################");

	try {
	    errorMessage = "Seems like IF-MIB::ifPhysAddress.1 (.1.3.6.1.2.1.2.2.1.6) providing wrong WAN MAC Address.";
	    /**
	     * Step 1 : Retrieve the Mac Address from the response of IF-MIB::ifPhysAddress.1 command and verify with
	     * the actual MAC Address of the device using device object
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1:DESCRIPTION: Retrieve the Mac Address from the response of IF-MIB::ifPhysAddress.1(for fibre device getting the erouter0 mib index) command and verify with the actual MAC Address of the device using device object");
	    LOGGER.info(
		    "STEP 1:ACTION: Execute SNMP command to retrive the WAN MAC Address by using SNMP and compare with Wan Mac address retrived using device object");
	    LOGGER.info("STEP 1:EXPECTED: Should return the Device WAN MAC Address");
	    LOGGER.info("**********************************************************************************");
	    ifTableIndex = BroadBandCommonUtils.getIndexForWanMac(tapEnv, device);
	    if (CommonMethods.isNotNull(ifTableIndex)) {
		snmpWanMacAddress = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_IF_MIB_IF_PHYSICAL_ADDRESS.getOid(), ifTableIndex);
		LOGGER.info("SNMP command output for IF-MIB::ifPhysAddress." + ifTableIndex
			+ "(.1.3.6.1.2.1.2.2.1.6) : " + snmpWanMacAddress);
		wanMacAddress = BroadBandSnmpUtils.formatMacAddressWithoutLeadingZeros(device.getHostMacAddress());
		if (CommonMethods.isNotNull(snmpWanMacAddress) && CommonMethods.isNotNull(wanMacAddress)) {
		    // Verify the retrieved MAC with the actual MAC address of the
		    // device
		    status = CommonUtils.patternSearchFromTargetString(snmpWanMacAddress, wanMacAddress);
		    if (status) {
			LOGGER.info(
				"STEP 1: ACTUAL : retreived Wan MAC address using SNMP and WAN MAC Address using device object are same ");
		    } else {
			LOGGER.error("STEP 1: ACTUAL :" + errorMessage + " ACTUAL OUTPUT :  WAN MAC Address :  "
				+ snmpWanMacAddress + " " + " EXPECTED OUTPUT: WAN MAC Address  : " + wanMacAddress);
		    }
		} else {
		    status = false;
		    LOGGER.error("Unable to retrieve IF-MIB::ifPhysAddress." + ifTableIndex
			    + "(.1.3.6.1.2.1.2.2.1.6) details using SNMP MIB");
		}
	    } else {
		LOGGER.error(
			"Index to get WAN MAC address is null. Hence Will not able to get WAN MAC Address using SNMP.");
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, true);
	} catch (Exception exception) {
	    status = false;
	    LOGGER.error("Unable to retrieve IF-MIB::ifPhysAddress." + ifTableIndex
		    + "(.1.3.6.1.2.1.2.2.1.6) details using SNMP MIB" + exception.getMessage());
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    true);
	}
    }

    /**
     * Test to Verify the Cable Modem MAC Address using IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) SNMP MIB.
     * 
     * @param device
     *            The Device to be used.
     * @author Selvaraj Mariyappan
     * @Refactor Athira
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.SNMP_OPERATIONS })
    @TestDetails(testUID = "TC-RDKB-SNMP-1004")

    public void testSnmpGetOnCableModemMacAddress(Dut device) {
	String testCaseId = "TC-RDKB-SNMP-004";
	String stepNumber = "s1";
	boolean status = false;
	String message = null;
	String snmpCableModemMacAddress = null;
	String cableModemMacAddress = null;

	LOGGER.info("#######################################################################################");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the Cable Modem MAC Address using IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2)");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1:Retrieve the CM Mac Address from the response of IF-MIB::ifPhysAddress.2 command and verify with the actual MAC Address of the device using device object");
	LOGGER.info("#######################################################################################");

	/**
	 * Step 1 : Retrieve the CM Mac Address from the response of IF-MIB::ifPhysAddress.2 command and verify with the
	 * actual MAC Address of the device using device object
	 */

	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP 1: DESCRIPTION: Retrieve the CM Mac Address from the response of IF-MIB::ifPhysAddress.2 command and verify with the actual MAC Address of the device using device object");
	LOGGER.info(
		"STEP 1:ACTION: Execute SNMP command to retrive the CM MAC Address by using SNMP oid .1.3.6.1.2.1.2.2.1.6.2");

	LOGGER.info("EXPECTED: Should return the Device Cable modem address");
	LOGGER.info("**********************************************************************************");
	try {

	    // Retrieve the Cable Modem Mac Address from the SNMP command
	    snmpCableModemMacAddress = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
		    BroadBandSnmpMib.ECM_IF_MIB_IF_PHYSICAL_ADDRESS.getOid(), "2");
	    LOGGER.info("SNMP command output for IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) : "
		    + snmpCableModemMacAddress);
	    /*
	     * Cable-modem MAC address is retrieved from BHC.
	     */
	    cableModemMacAddress = BroadBandSnmpUtils
		    .formatMacAddressWithoutLeadingZeros(((Device) device).getEcmMac());

	    message = " EXPECTED: Cable Modem MAC Address  : " + cableModemMacAddress;

	    if (CommonMethods.isNotNull(snmpCableModemMacAddress) && CommonMethods.isNotNull(cableModemMacAddress)) {
		// Verify the retrieved MAC with the actual MAC from CHIMPS
		status = (snmpCableModemMacAddress.trim()).equalsIgnoreCase(cableModemMacAddress.trim());

		if (!status) {
		    message = "Seems like IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) providing wrong Cable Modem Address . ACTUAL :  Cable Modem Address :  "
			    + snmpCableModemMacAddress + message;
		    LOGGER.error(message);
		}
	    } else {
		status = false;
		message = "Unable to retrieve Cable Modem Address using IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) SNMP MIB";
		LOGGER.error(message);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + message, true);
	} catch (Exception exception) {
	    status = false;
	    message = "Unable to retrieve IF-MIB::ifPhysAddress.2 (.1.3.6.1.2.1.2.2.1.6.2) details using SNMP MIB"
		    + exception.getMessage();
	    LOGGER.error(message);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, message, true);
	}
    }

    /**
     * Test to Verify the Current software version using DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers(
     * 1.3.6.1.2.1.69.1.3.5.0) SNMP MIB.
     * 
     * @param device
     *            The device to be used.
     * 
     * @author Selvaraj Mariyappan
     * @Refactor Athira
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.SNMP_OPERATIONS })
    @TestDetails(testUID = "TC-RDKB-SNMP-1005")
    public void testSnmpGetOnDocsisCableDeviceCurrentSoftwareVersion(Dut device) {

	String testCaseId = "TC-RDKB-SNMP-005";
	String stepNumber = "s1";
	boolean status = false;
	String message = null;
	String snmpDocsisDeviceCurrentSoftwareVersion = null;
	String currentDocsisSwVers = null;
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1005");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the Current software version using DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0)");
	LOGGER.info(
		"1. Retrieve the Software version from the response of DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers command and verify with the actual SW version of the device using device object");
	LOGGER.info("#######################################################################################");

	/**
	 * Step 1 : Retrieve the Software version from the response of DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers
	 * command and verify with the actual SW version of the device using device object
	 */

	LOGGER.info("**********************************************************************************");
	LOGGER.info(
		"STEP 1: Retrieve the Software version from the response of DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers command and verify with the actual SW version of the device using device object");
	LOGGER.info("EXPECTED: Should return the current software version");
	LOGGER.info("**********************************************************************************");

	try {
	    // Retrieve the software version from the SNMP command -
	    // DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers
	    snmpDocsisDeviceCurrentSoftwareVersion = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
		    BroadBandSnmpMib.ECM_DOCS_DEV_CURRENT_SOFTWARE_VERSION.getOid());
	    LOGGER.info(
		    "SNMP command output for DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) : "
			    + snmpDocsisDeviceCurrentSoftwareVersion);

	    currentDocsisSwVers = BroadBandSnmpUtils.getExpectedDocsisCableDeviceCurrentSoftwareVersion(tapEnv, device);

	    message = "\t EXPECTED: Currently running software version  : " + currentDocsisSwVers;

	    if (CommonMethods.isNotNull(snmpDocsisDeviceCurrentSoftwareVersion)
		    && CommonMethods.isNotNull(currentDocsisSwVers)) {
		// Verify the retrieved version with the actual firmware version
		// of the device
		status = (snmpDocsisDeviceCurrentSoftwareVersion.trim()).equalsIgnoreCase(currentDocsisSwVers.trim());
		if (!status) {
		    message = "Seems like DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) providing wrong currently running software version. ACTUAL : DOCSIS software version :  "
			    + snmpDocsisDeviceCurrentSoftwareVersion + message;
		    LOGGER.error(message);
		}
	    } else {
		status = false;
		message = "Unable to retrieve Currently running software version using DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) SNMP MIB";
		LOGGER.error(message);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + message, true);
	} catch (Exception exception) {
	    status = false;
	    message = "Unable to retrieve DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0) details using SNMP MIB"
		    + exception.getMessage();
	    LOGGER.error(message);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, message, true);
	}
    }

    /**
     * Test to verify deviceUpTime using SysUpTime( 1.3.6.1.2.1.1.3.0 ) SNMP MIB and cross check it with output of 'cat
     * /proc/uptime' command
     * 
     * @author Karthick Pandiyan
     * @Refactor Athira
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.SNMP_OPERATIONS })
    @TestDetails(testUID = "TC-RDKB-SNMP-1006")
    public void testDeviceUpTime(Dut device) {

	String testStepNumber = "s1";
	String testId = "TC-RDKB-SNMP-006";
	String errorMessage = null;
	boolean status = false;

	LOGGER.info("*****************************************************************************************");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1006");
	LOGGER.info(
		" TEST DESCRIPTION: Verify the Current software version using DOCS-CABLE-DEVICE-MIB::docsDevSwCurrentVers( 1.3.6.1.2.1.69.1.3.5.0)");
	LOGGER.info(
		"STEP 1: Verify deviceUpTime using SysUpTime( 1.3.6.1.2.1.1.3.0 )SNMP MIB and cross check it with output of cat /proc/uptime command");
	LOGGER.info("*****************************************************************************************");

	LOGGER.info("*****************************************************************************************");
	LOGGER.info(
		"STEP 1: Test to Verify deviceUpTime using SysUpTime( 1.3.6.1.2.1.1.3.0 )SNMP MIB and cross check it with output of cat /proc/uptime command");
	LOGGER.info("EXPECTED: SysUpTime difference between SNMP and cat /proc/uptime should be less than two minutes");
	LOGGER.info("*****************************************************************************************");

	try {
	    errorMessage = "Failed to verify system uptime ";
	    status = BroadBandSnmpUtils.verifySysUpTime(tapEnv, device);

	    if (!status) {
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("ACTUAL: Verification of system uptime : " + status);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    status = false;
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-1006");

    }

    /**
     * Test to verify Support Legacy DOCSIS SNR OID
     * 
     * <ol>
     * <li>STEP 1: Execute SNMPWALK command on OID docsIfSigQSignalNoise ( 1.3.6.1.2.1.10.127.1.1.4.1.5).</li>
     * <li>STEP 2: Execute SNMP v3 SET command on OID docsIfSigQSignalNoise(1.3.6.1.2.1.10.127.1.1.4.1.5) which is a
     * READ-ONLY MIB</li>
     * </ol>
     * 
     * @param device
     *            The Device to be used.
     * @author Deepa Bada
     * @Refactor Athira
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.SNMP_OPERATIONS })
    @TestDetails(testUID = "TC-RDKB-SNMP-SIGTONOISE-1021")
    public void testSnmpWalk(Dut device) {
	String testCaseId = "TC-RDKB-SNMP-SIGTONOISE-121";
	String stepNumber = "s1";
	boolean status = false;
	String errorMessage = "Failed to verify DOCSIF SigQSignal Noise";

	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-SIGTONOISE-1021");
	    LOGGER.info(
		    "TEST DESCRIPTION:Verify the values of DOCS-IF-MIB::docsIfSigQSignalNoise in the given MIB range");
	    LOGGER.info(
		    "NOTES :Test case is created as part of NEW FEATURE AUTOMATION based on RDKB-14936 - [New Feature Automation]");
	    LOGGER.info("TEST STEPS : ");

	    LOGGER.info("1: Execute SNMPWALK command on OID docsIfSigQSignalNoise (1.3.6.1.2.1.10.127.1.1.4.1.5)");
	    LOGGER.info(
		    "2: Execute SNMP v3 SET command on OID docsIfSigQSignalNoise(1.3.6.1.2.1.10.127.1.1.4.1.5) which is a READ-ONLY");
	    LOGGER.info("#######################################################################################");

	    /**
	     * Step 1 : Execute SNMPWALK command on OID docsIfSigQSignalNoise(1.3.6.1.2.1.10.127.1.1.4.1.5)
	     * 
	     */

	    LOGGER.info("######################################################");
	    LOGGER.info(
		    "STEP 1 : DESCRIPTION: Verify the values of DOCS-IF-MIB::docsIfSigQSignalNoise in the given MIB range(MIB>=200 and MIB <=500)");
	    LOGGER.info("STEP 1 : ACTION: Execute snmpwalk -v2c -c <> <ecm IP> 1.3.6.1.2.1.10.127.1.1.4.1.5");
	    LOGGER.info("STEP 1 : EXPECTED: Should return the list of child OID's");
	    LOGGER.info("######################################################");
	    status = BroadBandSnmpUtils.verifyDocsIfSigQSignalNoiseWithRange(tapEnv, device);

	    LOGGER.info(
		    "STEP 1: ACTUAL: Verification of OID(DOCSIF SigQSignal Noise) value in range of greater than equal to 200 & less than equal to 500 : "
			    + status);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(ErrorType.SNMP.toString(), errorMessage), false);

	    /**
	     * Step 2 : Execute SNMP v3 SET command on OID (1.3.6.1.2.1.10.127.1.1.4.1.5) which is a READ-ONLY MIB
	     * 
	     */

	    LOGGER.info("######################################################");
	    LOGGER.info("STEP 2 : DESCRIPTION: Verify the READ-ONLY attribute of  DOCS-IF-MIB::docsIfSigQSignalNoise ");
	    LOGGER.info(
		    "STEP 2 : ACTION: Execute SNMP v3 SET command on OID docsIfSigQSignalNoise(1.3.6.1.2.1.10.127.1.1.4.1.5) which is a READ-ONLY MIB");
	    LOGGER.info("STEP 2 : EXPECTED   : Set should not be successful as it’s a read only MIB");
	    LOGGER.info("######################################################");

	    stepNumber = "s2";
	    status = false;

	    errorMessage = "Able to do SNMP  SET operation.SNMP Set operation on OID (1.3.6.1.2.1.10.127.1.1.4.1.5)  is expected to fail.";

	    status = BroadBandSnmpUtils.verifyReadOnlySignalNoiseStatus(device, tapEnv);

	    LOGGER.info("STEP 2: ACTUAL: "
		    + (status ? "The READ-ONLY MIB can not be writable through SNMP" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(ErrorType.SNMP.toString(), errorMessage), false);
	} catch (Exception testException) {
	    errorMessage = "Exception occurred validating OID docsIfSigQSignalNoise (1.3.6.1.2.1.10.127.1.1.4.1.5)"
		    + testException.getMessage();
	    LOGGER.error(errorMessage);

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(ErrorType.SNMP.toString(), errorMessage), false);
	}
    }

    /**
     * Verify the Software file name, Server transport protocol, Server address type and Admin status can be retieved
     * from SNMP
     * <ol>
     * <li>Verify the Software File name can be retrieved using SNMP</li>
     * <li>Verify the Server Transport Protocol can be retrieved using SNMP</li>
     * <li>Verify the Server Address Type can be retrieved using SNMP</li>
     * <li>Verify the Admin status can be retrieved using SNMP</li>
     * </ol>
     * 
     * @Refactor Athira
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SNMP_OPERATIONS)
    @TestDetails(testUID = "TC-RDKB-SNMP-5001")
    public void verifySnmpGetOnAdminMibs(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String response = null;
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-SNMP-501";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-5001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the Software file name, Server transport protocol, Server address type and Admin status can be retieved from SNMP");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the Software File name can be retrieved using SNMP");
	LOGGER.info("2. Verify the Server Transport Protocol can be retrieved using SNMP");
	LOGGER.info("3. Verify the Server Address Type can be retrieved using SNMP");
	LOGGER.info("4. Verify the Admin status can be retrieved using SNMP");
	LOGGER.info("#######################################################################################");

	try {
	    stepNum = "S1";
	    errorMessage = "Unable to set/get Software file name from SNMP";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify the Software File name can be retrieved using SNMP");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the SNMP command to retrive the Software File name using OID .1.3.6.1.2.1.69.1.3.2.0");
	    LOGGER.info("STEP 1: EXPECTED : Software file name should be retrieved from SNMP");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Setting Software File Name inorder to retrieve the same using SNMP");
	    String fileName = BroadBandCommonUtils.concatStringUsingStringBuffer(device.getFirmwareVersion(),
		    BroadBandTestConstants.BINARY_BUILD_IMAGE_EXTENSION);

	    String snmpSetResponse = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, device,
		    BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME.getOid() + ".0", SnmpDataType.STRING, fileName);

	    errorMessage = "snmpset on docsDevSwFilename(1.3.6.1.2.1.69.1.3.2.0) with current1 image version as file name failed";
	    if (CommonMethods.isNotNull(snmpSetResponse) && snmpSetResponse.equalsIgnoreCase(fileName)) {

		response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
			BroadBandSnmpMib.ECM_DOCS_DEV_SW_FILE_NAME.getOid() + ".0");

		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(fileName);
		errorMessage = "Software Version retrieved from SNMP is null/invalid. Value retrieved from SNMP : "
			+ response;
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Software file name has been successfully retrieved from SNMP");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S2";
	    errorMessage = "Unable to retrieve Server Transport Protocol from SNMP";
	    status = false;
	    response = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify the Server Transport Protocol can be retrieved using SNMP");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the SNMP command to retrive the Server Transport Protocol using OID 1.3.6.1.2.1.69.1.3.8.0");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Server Transport protocol should be retrieved from SNMP, the value should be either 1 (TFTP) or 2(HTTP)");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
		    BroadBandSnmpMib.ECM_SERVER_TRANSPORT_PROTOCOL.getOid() + ".0");

	    status = CommonMethods.isNotNull(response)
		    && (response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE)
			    || response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO));
	    errorMessage = "Server Transport protocol retrieved from SNMP is null/invalid. Value retrieved from SNMP : "
		    + response;
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : Server Transport protocol has been successfully retrieved from SNMP & the value is between 1(TFTP) and 2(HTTP)");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S3";
	    errorMessage = "Unable to retrieve Server Address Type from SNMP";
	    status = false;
	    response = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify the Server Address Type can be retrieved using SNMP");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the SNMP command to retrieve Server Address Type using OID 1.3.6.1.2.1.69.1.3.6.0");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Server Address Type should be retrieved from SNMP, the value should be either 0(IPv4) or 1 (IPv6) or 2(Hexa Decimal)");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
		    BroadBandSnmpMib.ECM_SERVER_ADDRESS_TYPE_CDL.getOid() + ".0");

	    status = CommonMethods.isNotNull(response) && (response.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO)
		    || response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE)
		    || response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO));
	    errorMessage = "Server Address Type retrieved from SNMP is null/invalid. Value retrieved from SNMP : "
		    + response;
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Server Address Type has been successfully retrieved from SNMP & the value is either 0(IPv4) or 1 (IPv6) or 2(Hexa Decimal)");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S4";
	    errorMessage = "Unable to retrieve Admin status from SNMP";
	    status = false;
	    response = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify the Admin status can be retrieved using SNMP");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the SNMP command to retrieve Admin status using OID 1.3.6.1.2.1.69.1.3.3.0");
	    LOGGER.info(
		    "STEP 4: EXPECTED : Admin status should be retrieved from SNMP, the value should be either 1 or 2 or 3");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandSnmpUtils.snmpGetOnEcm(tapEnv, device,
		    BroadBandSnmpMib.ECM_DOCS_DEVSW_ADMIN_STATAUS.getOid() + ".0");

	    status = CommonMethods.isNotNull(response)
		    && (response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE)
			    || response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO)
			    || response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_THREE));
	    errorMessage = "Admin status retrieved from SNMP is null/invalid. Value retrieved from SNMP : " + response;
	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : Admin status has been successfully retrieved from SNMP & the value is either 1 or 2 or 3");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-5001");
    }

    /**
     * Test to verify DOCSIS Signal Quality Extended RxMER Table MIB values and its READ-ONLY attribute
     * 
     * <ol>
     * <li>STEP 1: Verify the values of DOCSIS Signal Quality Extended RxMER Table MIB are greater than 100</li>
     * <li>STEP 2: Verify the READ-ONLY attribute of docsIf3SignalQualityExtRxMER MIB</li>
     * </ol>
     * 
     * @param device
     *            The device to be used.
     * @Refactor Alan_Bivera
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.SNMP_OPERATIONS })
    @TestDetails(testUID = "TC-RDKB-SNMP-1008", testDecription = "Verify DOCSIS Signal Quality Extended RxMER Table MIB values and its READ-ONLY attribute")
    public void testSnmpWalkOnDocsisSignalQualityExtendedRxMerTable(Dut device) {

	String testCaseId = "TC-RDKB-SNMP-108";
	String stepNumber = "s1";
	boolean status = false; // stores the test status
	String errorMessage = null; // stores the error message

	try {
	    String snmpWalkOutput = null; // stores SNMP walk output

	    /**
	     * Step 1 : Verify the values of DOCSIS Signal Quality Extended RxMER Table MIB are greater than 100
	     * 
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: Verify the values of DOCSIS Signal Quality Extended RxMER Table MIB are greater than 100");
	    LOGGER.info(
		    "EXPECTED: On executing this command, SNMP should return a list of values under docsIf3SignalQualityExtRxMER MIB table and all the returned values should be greater than 100");
	    LOGGER.info("**********************************************************************************");
	    snmpWalkOutput = BroadBandSnmpUtils.snmpWalkOnEcm(tapEnv, device,
		    BroadBandSnmpMib.ECM_DOCSIS_SIGNAL_QUALITY_EXTENDED_RX_MER_TABLE.getOid());
	    status = CommonMethods.isNotNull(snmpWalkOutput) && BroadBandSnmpUtils
		    .validateSnmpOutputWithExpectedValue(snmpWalkOutput, BroadBandTestConstants.CONSTANT_100);
	    errorMessage = "Invalid/Null value retrieved for docsIf3SignalQualityExtRxMER table";
	    LOGGER.info("S1 ACTUAL: "
		    + (status ? "All the values under docsIf3SignalQualityExtRxMER table are greater than 100"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    /**
	     * Step 2 : Verify the READ-ONLY attribute of docsIf3SignalQualityExtRxMER MIB
	     * 
	     */
	    stepNumber = "s2";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: Verify the READ-ONLY attribute of docsIf3SignalQualityExtRxMER MIB");
	    LOGGER.info("EXPECTED: Set should not be successful as its a read only MIB");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.isNotNull(snmpWalkOutput)
		    && BroadBandSnmpUtils.validateReadOnlyAttributeOFDocsisSignalQualityTables(device, tapEnv,
			    snmpWalkOutput, BroadBandSnmpMib.ECM_DOCSIS_SIGNAL_QUALITY_EXTENDED_RX_MER_TABLE.getOid(),
			    SnmpDataType.INTEGER, BroadBandTestConstants.STRING_VALUE_THREE_HUNDRED_AND_EIGHTY);
	    errorMessage = "docsIf3SignalQualityExtRxMER MIB can be writable";
	    LOGGER.info(
		    "S2 ACTUAL: " + (status ? "All the child OID's under docsIf3SignalQualityExtRxMER MIB are READ ONLY"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

	} catch (Exception testException) {
	    errorMessage = "Exception occured while trying to verify DOCSIS Signal Quality Extended RxMER Table MIB values and its READ-ONLY attribute"
		    + testException.getMessage();
	    LOGGER.error(errorMessage);

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	}

    }

    /**
     * Test to verify DOCSIS Signal Quality Extended RxMER Samples Table MIB values and its READ-ONLY attribute
     * 
     * <ol>
     * <li>STEP 1: Verify the values of DOCSIS Signal Quality Extended RxMER Samples Table MIB are greater than 0</li>
     * <li>STEP 2: Verify the READ-ONLY attribute of docsIf3SignalQualityExtRxMERSamples MIB</li>
     * </ol>
     * 
     * @param device
     *            The device to be used.
     * @Refactor Alan_Bivera
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.SNMP_OPERATIONS })
    @TestDetails(testUID = "TC-RDKB-SNMP-1009", testDecription = "Verify DOCSIS Signal Quality Extended RxMER Samples Table MIB values and its READ-ONLY attribute")
    public void testSnmpWalkOnDocsisSignalQualityExtendedRxMerSamplesTable(Dut device) {

	String testCaseId = "TC-RDKB-SNMP-109";
	String stepNumber = "s1";
	boolean status = false; // stores the test status
	String errorMessage = null; // stores the error message

	try {
	    String snmpWalkOutput = null; // stores SNMP walk output

	    /**
	     * Step 1 : Verify the values of DOCSIS Signal Quality Extended RxMER Samples Table MIB are greater than 100
	     * 
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: Verify the values of DOCSIS Signal Quality Extended RxMER Samples Table MIB are greater than 0");
	    LOGGER.info(
		    "EXPECTED: On executing this command, SNMP should return a set of values under docsIf3SignalQualityExtRxMerSamples MIB table and all the returned values should be greater than 0");
	    LOGGER.info("**********************************************************************************");
	    snmpWalkOutput = BroadBandSnmpUtils.snmpWalkOnEcm(tapEnv, device,
		    BroadBandSnmpMib.ECM_DOCSIS_SIGNAL_QUALITY_EXTENDED_RX_MER_SAMPLES_TABLE.getOid());
	    status = CommonMethods.isNotNull(snmpWalkOutput) && BroadBandSnmpUtils
		    .validateSnmpOutputWithExpectedValue(snmpWalkOutput, BroadBandTestConstants.CONSTANT_0);
	    errorMessage = "Invalid/Null value retrieved for docsIf3SignalQualityExtRxMerSamples table";
	    LOGGER.info("S1 ACTUAL: "
		    + (status ? "All the values under docsIf3SignalQualityExtRxMerSamples table are greater than 0"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    /**
	     * Step 2 : Verify the READ-ONLY attribute of docsIf3SignalQualityExtRxMERSamples MIB
	     * 
	     */
	    stepNumber = "s2";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: Verify the READ-ONLY attribute of docsIf3SignalQualityExtRxMerSamples MIB");
	    LOGGER.info("EXPECTED: Set should not be successful as its a read only MIB");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.isNotNull(snmpWalkOutput) && BroadBandSnmpUtils
		    .validateReadOnlyAttributeOFDocsisSignalQualityTables(device, tapEnv, snmpWalkOutput,
			    BroadBandSnmpMib.ECM_DOCSIS_SIGNAL_QUALITY_EXTENDED_RX_MER_SAMPLES_TABLE.getOid(),
			    SnmpDataType.UNSIGNED_INTEGER,
			    BroadBandTestConstants.STRING_VALUE_FIVE_LAKH_TWENTY_FOUR_THOUSAND);
	    errorMessage = "docsIf3SignalQualityExtRxMERSamples MIB can be writable";
	    LOGGER.info("S2 ACTUAL: "
		    + (status ? "All the child OID's under docsIf3SignalQualityExtRxMerSamples MIB are READ ONLY"
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, ErrorType.SNMP + errorMessage, true);

	} catch (Exception testException) {
	    errorMessage = "Exception occured while trying to verify DOCSIS Signal Quality Extended RxMER Samples Table MIB values and its READ-ONLY attribute"
		    + testException.getMessage();
	    LOGGER.error(errorMessage);

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	}

    }

    /**
     * Verify Ethernet Related SNMP
     * <ol>
     * <li>Verify the ethernet port link speed using SNMP MIB.</li>
     * <li>Verify the Date using SNMP MIB .</li>
     * <li>Verify device's maximum CEP using SNMP MIB.</li>
     * <li>Verify DHCP server type of device using SNMP MIB.</li>
     * <li>Verify the device's DHCP server address.</li>
     * <li>Verify device's software operation status.</li>
     * 
     * @author Revanth Kumar Vella
     * @Refactor Alan_Bivera
     *           </ol>
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.SNMP_OPERATIONS })
    @TestDetails(testUID = "TC-RDKB-SNMP-1026", testDecription = "Verify Ethernet Related SNMP")
    public void verifyEthernetSnmp(Dut device) {

	// Variable Declaration begins
	String testCaseId = null;
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	String response = null;
	// Variable Declation Ends

	testCaseId = "TC-RDK-SNMP-126";
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-XB-SNMP-1026");
	LOGGER.info("TEST DESCRIPTION: Verify Ethernet Related SNMP");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify ethernet port link speed using SNMP MIB.");
	LOGGER.info("2. Verify the Date using SNMP MIB .");
	LOGGER.info("3. Verify device's maximum CEP using SNMP MIB.");
	LOGGER.info("4. Verify DHCP server type of device using SNMP MIB.");
	LOGGER.info("5. Verify the device's DHCP server address.");
	LOGGER.info("6. Verify device's software operation status.");

	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s1";
	    errorMessage = "Failed to get ethernet port link speed using SNMP MIB(1.3.6.1.2.1.2.2.1.5.1).";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify the ethernet port link speed using SNMP MIB.");
	    LOGGER.info(
		    "STEP 1: ACTION : Get the field value for ethernet port link speed using SNMP MIB(1.3.6.1.2.1.2.2.1.5.1).");
	    LOGGER.info(
		    "STEP 1: EXPECTED : Ethernet port link speed should return value of Integer like 0(0 Mbps), 10000000(10 Mbps), 100000000(100 Mbps) or 1000000000(1000Mbps).");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_CM_ETHERNET_OPER_SETTING.getOid(),
			BroadBandSnmpMib.ECM_CM_ETHERNET_OPER_SETTING.getTableIndex());
		if (CommonMethods.isNotNull(response)
			&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE))
			&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_INSTANCE))) {
		    // Validate whether the Eternet port link speed returned is valid.
		    status = BroadBandSnmpUtils.validateCmEthernetOperSettingValueRetrievedFromSnmp(response);
		} else {
		    errorMessage = "Obtained null response/No such oid for  snmp mib execution to get ethernet port speed";
		    LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
		}
		if (status) {
		    LOGGER.info("STEP 1: ACTUAL : SUCCESSFULLY VERIFIED ETHERNET PORT LINK SPEED AS " + response);
		} else {
		    errorMessage = "Expected ethernet port link speed value should be "
			    + "like 0(0 Mbps), 10000000(10 Mbps), 100000000(100 Mbps) or 1000000000(1000Mbps). But obtained response is : "
			    + response;
		    LOGGER.error("STEP 1: ACTUAL :" + errorMessage);
		}
	    } catch (TestException exception) {
		errorMessage = "Exception occured while geting Ethernet port link speed : " + exception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s2";
	    errorMessage = "Failed to get Date using SNMP MIB(.1.3.6.1.2.1.69.1.1.2.0).";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify the Date using SNMP MIB .");
	    LOGGER.info("STEP 2: ACTION : Get the field value for Date using SNMP MIB (.1.3.6.1.2.1.69.1.1.2.0).");
	    LOGGER.info(
		    "STEP 2: EXPECTED : A valid Date should be displayed. It should return value of type Hex-STRING.");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_DATE.getOid(), BroadBandSnmpMib.ECM_DATE.getTableIndex());
		status = CommonMethods.isNotNull(response)
			&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE))
			&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_INSTANCE))
			&& !BroadBandCommonUtils.patternSearchFromTargetString(response,
				BroadBandTestConstants.SNMPV3_TIMEOUT_ERROR)
			&& CommonMethods.patternMatcher(response, BroadBandTestConstants.HEXA_STRING_REGEX);
		if (status) {
		    LOGGER.info(
			    "STEP 2: ACTUAL : SUCCESSFULLY OBTAINED FIELD VALUE FOR DATE USING SNMP MIB(.1.3.6.1.2.1.69.1.1.2.0) : "
				    + response);
		} else {
		    errorMessage = "Obtained null response/No such oid for  snmp mib execution to get Date";
		    LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    } catch (TestException exception) {
		errorMessage = "Exception occured while geting Date value : " + exception.getMessage();
		LOGGER.error(errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    stepNum = "s3";
	    errorMessage = "Failed to get device's maximum CEP using SNMP MIB (.1.3.6.1.2.1.69.1.1.7.0).";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify device's maximum CEP using SNMP MIB.");
	    LOGGER.info(
		    "STEP 3: ACTION : Get the field value for maximum CEP using SNMP MIB  (.1.3.6.1.2.1.69.1.1.7.0).");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Device's maximum CEP should return value of type integer value like 1 or 5.");
	    LOGGER.info("**********************************************************************************");
	    try {
		long startTime = System.currentTimeMillis();
		do {
		    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			    BroadBandSnmpMib.ECM_DEV_MAX_CPE.getOid(),
			    BroadBandSnmpMib.ECM_DEV_MAX_CPE.getTableIndex());
		    if (CommonMethods.isNotNull(response)) {
			// initially response is compared between static values 1 and 5,now it is modified as
			// comparison between snmp and webpa response.
			errorMessage = "Expected Device's maximum CEP value using snmp and webpa are not same";
			status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_TO_CHECK_CPE_VALUE, response);
		    }
		} while (!status
			&& (System.currentTimeMillis() - startTime) < BroadBandTestConstants.ONE_MINUTE_IN_MILLIS
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS));
		if (status) {
		    LOGGER.info("STEP 3: ACTUAL : SUCCESSFULLY VERIFIED THE FIELD VALUE FOR MAXIMUM CEP OF DEVICE AS "
			    + response);
		} else {
		    LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    } catch (TestException exception) {
		errorMessage = "Exception occured while geting Device's maximum CEP : " + exception.getMessage();
		LOGGER.error(errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    stepNum = "s4";
	    errorMessage = "Failed to get DHCP server type of device using SNMP MIB(.1.3.6.1.2.1.69.1.4.6.0).";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify DHCP server type of device using SNMP MIB.");
	    LOGGER.info(
		    "STEP 4: ACTION : Get the field value for DHCP server type using SNMP MIB  (.1.3.6.1.2.1.69.1.4.6.0).");
	    LOGGER.info(
		    "STEP 4: EXPECTED : A valid DHCP server type should be displayed. It should return value of type integer 2.");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_DEV_SERVER_DHCP_TYPE.getOid(),
			BroadBandSnmpMib.ECM_DEV_SERVER_DHCP_TYPE.getTableIndex());
		if (CommonMethods.isNotNull(response)) {
		    // Validate whether the DHCP server type returned is valid.
		    status = response.equals(BroadBandTestConstants.STRING_VALUE_TWO);
		    if (status) {
			LOGGER.info("STEP 4: ACTUAL : SUCCESSFULLY VERIFIED VALUE OF DEVICE'S DHCP SERVER TYPE AS "
				+ response);
		    } else {
			errorMessage = "Expected DHCP server type should be " + "2. But obtained response is : "
				+ response;
			LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		    }
		} else {
		    errorMessage = "Obtained null response/No such oid for snmp mib execution to get DHCP server type";
		    LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    } catch (TestException exception) {
		errorMessage = "Exception occured while geting DHCP server type : " + exception.getMessage();
		LOGGER.error(errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    stepNum = "s5";
	    errorMessage = "Failed to get device's DHCP server address using SNMP MIB( .1.3.6.1.2.1.69.1.4.7.0).";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify the device's DHCP server address.");
	    LOGGER.info(
		    "STEP 5: ACTION : Get the field value for DHCP server address using SNMP MIB( .1.3.6.1.2.1.69.1.4.7.0).");
	    LOGGER.info("STEP 5: EXPECTED : A DHCP server address should be displayed.");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_DEV_SERVER_DHCP_ADDRESS.getOid(),
			BroadBandSnmpMib.ECM_DEV_SERVER_DHCP_ADDRESS.getTableIndex());
		status = CommonMethods.isNotNull(response)
			&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_OBJECT_AVAILABLE))
			&& !(response.equalsIgnoreCase(BroadBandTestConstants.NO_SUCH_INSTANCE))
			&& !BroadBandCommonUtils.patternSearchFromTargetString(response,
				BroadBandTestConstants.SNMPV3_TIMEOUT_ERROR)
			&& CommonMethods.patternMatcher(response, BroadBandTestConstants.HEXA_STRING_REGEX);
		if (status) {
		    LOGGER.info(
			    "STEP 5: ACTUAL : SUCCESSFULLY OBTAINED FIELD VALUE FOR DHCP SERVER ADDRESS USING SNMP MIB( .1.3.6.1.2.1.69.1.4.7.0) : "
				    + response);
		} else {
		    errorMessage = "Obtained null response/No such oid for  snmp mib execution to get DHCP server address";
		    LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    } catch (TestException exception) {
		errorMessage = "Exception occured while geting DHCP server address : " + exception.getMessage();
		LOGGER.error(errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }

	    stepNum = "s6";
	    errorMessage = "Failed to get device's operation status value using SNMP MIB(.1.3.6.1.2.1.69.1.3.4.0).";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify device's software operation status value.");
	    LOGGER.info(
		    "STEP 6: ACTION : Get the field value for software operation status using SNMP MIB (.1.3.6.1.2.1.69.1.3.4.0).");
	    LOGGER.info(
		    "STEP 6: EXPECTED : A valid software operation status should be displayed. It should return value of type integer value like 1, 2, 3, 4 or 5.");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_DEV_SW_OPER_STATUS.getOid(),
			BroadBandSnmpMib.ECM_DEV_SW_OPER_STATUS.getTableIndex());
		if (CommonMethods.isNotNull(response)) {
		    // Validate whether the DevSwOperStatus returned is valid.
		    status = response.equals(BroadBandTestConstants.STRING_VALUE_ONE)
			    || response.equals(BroadBandTestConstants.STRING_VALUE_TWO)
			    || response.equals(BroadBandTestConstants.STRING_VALUE_THREE)
			    || response.equals(BroadBandTestConstants.STRING_VALUE_FOUR)
			    || response.equals(BroadBandTestConstants.STRING_VALUE_FIVE);
		    if (status) {
			LOGGER.info(
				"STEP 6: ACTUAL : SUCCESSFULLY VERIFIED FIELD VALUE FOR SOFTWARE OPERATION STATUS AS "
					+ response);
		    } else {
			errorMessage = "Expected software operation status should be like 3 or 5. But obtained response is : "
				+ response;
			LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
		    }
		} else {
		    errorMessage = "Obtained null response/No such oid for  snmp mib execution to get software operation status";
		    LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    } catch (TestException exception) {
		errorMessage = "Exception occured while geting software operation status : " + exception.getMessage();
		LOGGER.error(errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    }
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-1026");
    }

    /**
     * Test to validate that the Delegated IPv6 prefix can be retrieved using rdkbRgDeviceConfigStaticIpv6
     * (.1.3.6.1.4.1.17270.50.2.1.4.7.0)
     * 
     * Step 1 : Get the delegated IPv6 prefix of the device using SNMP get
     * 
     * Step 2: Validate the IP returned in the above response is in line with the device logs
     * 
     * Step 3: Validate the IP returned in the above response is in line with the TR-181 parameters
     * 
     * @param settop
     *            The settop to be used.
     * 
     * @author Sathurya Ravi
     * @Refactor Alan_Bivera
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.SNMP_OPERATIONS })
    @TestDetails(testUID = "TC-RDKB-SNMP-1025", testDecription = "Validate that the Delegated IPv6 prefix can be retrieved using  rdkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0)")
    public void testValidateStaticIPv6Address(Dut device) {
	// stores the test case id
	String testCaseId = "TC-RDKB-SNMP-125";
	// stores the step number
	int stepNumber = 1;
	// stores the test step number
	String step = "S" + stepNumber;
	// stores the test result
	boolean status = false;
	// stores the error message
	String errorMessage = "";
	// stores the command response
	String response = "";
	// stores staticIpAddress
	String staticIpAddress = "";
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-SNMP-1025");
	    LOGGER.info(
		    "TEST DESCRIPTION: Validate that the Delegated IPv6 prefix can be retrieved using  rdkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0)");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("Step 1. Get the delegated IPv6 prefix of the device using SNMP get");
	    LOGGER.info("Step 2. Validate the IP returned in the above response is in line with the device logs ");
	    LOGGER.info(
		    "Step 3. Validate the IP returned in the above response is in line with the TR-181 parameters ");
	    LOGGER.info("#######################################################################################");

	    /**
	     * Step 1 : Get the delegated IPv6 prefix of the device using SNMP get
	     */
	    stepNumber = 1;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Get the delegated IPv6 prefix of the device using SNMP get ");
	    LOGGER.info("STEP " + stepNumber
		    + ":ACTION: Excecute SNMP get on the MIB  rdkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0)");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: The SNMP get response should return the delegated IPv6 prefix of the device");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "The device has returned some Junk value for the dkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0)";
	    // Execute SNMP get operation for the MIB
	    // dkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0)
	    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
		    BroadBandSnmpMib.ECM_STATIC_WAN_IPV6.getOid(),
		    BroadBandSnmpMib.ECM_STATIC_WAN_IPV6.getTableIndex());
	    LOGGER.info("SNMP command output for dkbRgDeviceConfigStaticIpv6 (.1.3.6.1.4.1.17270.50.2.1.4.7.0) : "
		    + response);
	    if (CommonMethods.isNotNull(response)) {
		// Validate whether the IP address returned is valid.
		status = CommonMethods.isIpv6Address(response.substring(0, response.indexOf("/")));
		staticIpAddress = response;
		errorMessage = "The IP address returned is not in a proper format. Actual: " + response;

	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL: SNMP get response returned the delegated IPV6 prefix of the device" + response);
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage + response);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /**
	     * Step 2 : Validate the IP returned in the above response is in line with the device logs
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Validate the IP returned in the above response is in line with the device logs ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Parse through the output of the command cat /etc/dibbler/server.conf by SSHing to the device and get the static WAN IP.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: The parsed Static IP value and the value from the SNMP response should be the same");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "The delegated IPv6 prefix from the device logs is invalid";
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.COMMAND_TO_GET_IPV6_PREFIX_VALUE_ZEBRA);
	    status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response, staticIpAddress);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL: The parsed Static IP value and the value from the SNMP is same");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 3 : Validate the IP returned in the above response is in line with the TR-181 parameters
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Validate the IP returned in the above response is in line with the TR-181 parameters ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Parse through the output of the command dmcli eRT getv Device.IP.Interface.1.IPv6Prefix.1.Prefix and get the value of the delegated IPv6 prefix.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: The parsed Static IP value and the value from the SNMP response should be the same");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "The delegated IPv6 prefix from the TR-181 is invalid";
	    // Execute the command dmcli eRT getv
	    // Device.IP.Interface.1.IPv6Prefix.1.Prefix by SSHing to the device and
	    // get the static WAN IP address
	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DELEGATED_IPV6_PREFIX);
	    status = response.equalsIgnoreCase(staticIpAddress);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL: The parsed Static IP value and the value from the SNMP response is same");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
	} catch (Exception exception) {
	    status = false;
	    errorMessage = "Unable to validate that the delegated IPv6 prefix can be retrieved using  rdkbRgDeviceConfigStaticIp (.1.3.6.1.4.1.17270.50.2.1.4.7.0)"
		    + exception.getMessage();
	    LOGGER.error(errorMessage);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SNMP-1025");
    }

}
