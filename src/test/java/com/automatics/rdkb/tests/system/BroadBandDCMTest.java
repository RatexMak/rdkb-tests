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
package com.automatics.rdkb.tests.system;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.TypeReference;
import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;

/**
 * Class for Enable Disable Feature Via DCM
 */
public class BroadBandDCMTest extends AutomaticsTestBase{
    /**
     * Verify configurable RFC check-in trigger using tr181 parameter
     * <ol>
     * <li>Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow</li>
     * <li>Verify tr181 parameter has been enabled by RFC script executed after trigger</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * 
     * @author Ashwin sankara
     * @refactor Govardhan
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-RFC-1001")
    public void testVerifyConfigurableRFC(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	BroadBandResultObject result = new BroadBandResultObject();
	// Variable Declation Ends

	testCaseId = "TC-RDKB-RFC-101";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-RFC-1001");
	LOGGER.info("TEST DESCRIPTION: Verify configurable RFC check-in trigger using tr181 parameter");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	LOGGER.info("2. Verify tr181 parameter has been enabled by RFC script executed after trigger");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info(
		    "PRE-CONDITION : DESCRIPTION : Configure RFC payload data to enable tr181 parameter when RFC retrieve now is triggered.");
	    LOGGER.info(
		    "PRE-CONDITION : ACTION : Copy and update RFC_CONFIG_SERVER_URL in /nvram/rfc.properties and post the payload");
	    LOGGER.info("PRE-CONDITION : EXPECTED : Successfully copied file and posted payload data");

	    if (BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
		    AutomaticsPropertyUtility.getProperty(BroadBandTestConstants.PROP_KEY_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION)
			    .replace(BroadBandTestConstants.STRING_TO_CHANGE_ENCRYPT_CLOUD_UPLOAD_STATUS,
				    BroadBandTestConstants.TRUE))) {
		LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
	    }

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s1";
	    errorMessage = "Unable to set value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow to unsigned integer 1";
	    status = false;

	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Clear dcmrfc.log and trigger Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	    LOGGER.info(
		    "STEP 1: ACTION : Clear dcmrfc.log and execute webpa command to set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Control.RetrieveNow");
	    LOGGER.info("STEP 1: EXPECTED : Successfully set RFC.RetrieveNow parameter with unsigned int 1");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_IMMEDIATE_RFC_CHECK_IN, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.STRING_VALUE_ONE);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successfully set RFC.RetrieveNow parameter with unsigned int 1");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Failed to set parameter value using TR181 trigger of RFC";
	    status = false;

	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify tr181 parameter has been enabled by RFC script executed after trigger");
	    LOGGER.info(
		    "STEP 2: ACTION : Verify parameter value in rfc-parsed.txt, rfc_configdata.txt, dcmrfc.log and tr181 get.");
	    LOGGER.info("STEP 2: EXPECTED : Value of  EncryptUpload Enable parameter value is true");

	    result = BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
		    WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION,
		    BroadBandTestConstants.TRUE);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Value of  EncryptUpload Enable parameter value is true");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION : Remove nvram override of rfc.properties and disable tr181 parameter.");
	    LOGGER.info("POST-CONDITION : ACTION : Remove /nvram/rfc.properties and disable tr181 parameter.");
	    LOGGER.info("POST-CONDITION : EXPECTED : WebPA set is successful and parmeter value is false");

	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandRfcFeatureControlUtils.NVRAM_RFC_PROPERTIES)
		    && BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			    WebPaParamConstants.WEBPA_PARAM_TO_ENABLE_DISABLE_CLOUD_UPLOAD_ENCRYPTION,
			    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-RFC-1001");
	LOGGER.info("#######################################################################################");
    }
    
    /**
     * Test case is created as to Enable Disable Feature Via DCM
     *
     * Test Case # 1: Verify Enable Disable Feature Via DCM after Reboot
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * Precondition: Reboot the device
     * <li>STEP 1: Verify DCM RFC Service started successfully in ArmConsoleLog.txt.0 file</li>
     * <li>STEP 2: Verify Xconf server url in dcmrfc.log and dcm.properties</li>
     * <li>STEP 3: Verify device is querying DCM at startup</li>
     * <li>STEP 4: Verify json message is parsed successfully to file rfcresponse.json</li>
     * <li>STEP 5: Verify list of enabled feature from rfc_configdata.txt</li>
     * <li>STEP 6: Verify GET for TR-181 Parameters through WebPA command</li>
     * <li>STEP 7: Verify DCM response from log file dcmrfc.log</li>
     * </ol>
     *
     * @author Sumathi Gunasekaran 
     * @refactor yamini.s
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-1102")
    public void testEnableDisableFeatureViaDCMAfterReboot(Dut device) {
	// variable to store errorMessage
	String errorMessage = null;
	// variable to store testcaseID
	String testCaseId = "TC-RDKB-SYSTEM-102";
	// variable to store teststepNumber
	String testStepNumber = "s1";
	// variable to store status
	boolean status = false;
	// variable to store search command
	String searchCommand = null;
	// variable to store command output
	String commandOutput = null;
	// variable to store dcmServerUrl
	String dcmServerUrl = null;
	// Variable to store the response
	String response = null;
	// Variable to store log file name
	String logFileName = null;
	// Map to store Tr181 parameters and its value
	Map<String, Object> tr181ParametersMap = new HashMap<String, Object>();
	// boolean to notify whether rfc props or dcm props is to be used
	boolean hasRfcProps = false;

	try {
	    // Precondition
	    LOGGER.info("### Pre-Condition ### Going to reboot the device.");
	    BroadBandCommonUtils.rebootDeviceAsPreCondition(tapEnv, device);
	    LOGGER.info("Waiting till device uptime is 10 minutes.This is for all the logs to get updated ");
	    BroadBandCommonUtils.waitTillGivenBootupTime(device, tapEnv, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS);
	    LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-1102");
	    /**
	     * Step 1 : Verify DCM RFC Service started successfully in ArmConsoleLog.txt.0/Console.txt.0 file
	     *
	     */
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 1: Verify DCM RFC Service started successfully in ArmConsoleLog.txt.0 file");
	    LOGGER.info("Expected: Log message should be present in ArmConsolelog.txt.0 file.");
	    LOGGER.info("***********************************************************");
	    logFileName = (CommonMethods.isAtomSyncAvailable(device, tapEnv)) ? BroadBandCommandConstants.FILE_ARMCONSOLELOG
		    : BroadBandCommandConstants.FILE_CONSOLELOG;
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_DCM_RFC_STARTED, logFileName);
	    LOGGER.info("Response is: " + response);
	    status = CommonUtils.isNotEmptyOrNull(response);
	    errorMessage = "Unable to verify DCM RFC Service in log message.";
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : DCM RFC Service Started successfully..");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }	    
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 2 : Verify Xconf server url in dcmrfc.log and dcm.properties
	     *
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 2: Verify Xconf server url in dcmrfc.log and dcm.properties");
	    LOGGER.info("Expected: The value should be the same in dcm.properties and dcmrfc.log");
	    LOGGER.info("***********************************************************");
	    hasRfcProps = CommonUtils.isFileExists(device, tapEnv, BroadBandRfcFeatureControlUtils.ETC_RFC_PROPERTIES);
	    searchCommand = (hasRfcProps
		    ? BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
			    BroadBandRfcFeatureControlUtils.ETC_RFC_PROPERTIES)
		    : BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
			    BroadBandTestConstants.DCM_RFC_SERVER_URL_PROPERTY_NAME,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandCommandConstants.FILE_DCM_PROPERTIES));
	    LOGGER.info("Search Command is: " + searchCommand);
	    commandOutput = tapEnv.executeCommandUsingSsh(device, searchCommand);
	    if (CommonMethods.isNotNull(commandOutput)) {
		dcmServerUrl = (hasRfcProps
			? CommonMethods.patternFinder(commandOutput,
				BroadBandRfcFeatureControlUtils.PATTERN_FOR_RFC_CONFIG_SERVER_URL)
			: CommonMethods.patternFinder(commandOutput,
				BroadBandTestConstants.REG_EXPRESSION_DCM_SERVER_URL));
		errorMessage = "Unable to verify Xconf server url in log message.";
		if (CommonMethods.isNotNull(dcmServerUrl)) {
		    searchCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(
			    BroadBandTestConstants.GREP_COMMAND, dcmServerUrl,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.DCMRFC_LOG_FILE);
		    LOGGER.info("Search Command is :" + searchCommand);
		    status = CommonUtils.searchLogFiles(tapEnv, device, searchCommand);
		    errorMessage = "Xconf server url is different in dcm.properties file and in /rdklogs/logs/dcmRFC.log.0.";
		}
	    }
	   
	    if (!status) {
		status = BroadBandRfcFeatureControlUtils.validateRFCConfigURL(device,tapEnv);
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Xconf server url is verified successfully.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }		   
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    
	    /**
	     * Step 3 : Verify device is querying DCM at startup
	     *
	     */
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 3: Verify device is querying DCM at startup");
	    LOGGER.info("Expected: Log message should be present in dcmrfc.log");
	    LOGGER.info("***********************************************************");
	    searchCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
		    BroadBandTraceConstants.LOG_MESSAGE_CURL_COMMAND, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		    BroadBandCommandConstants.FILE_DCMRFC_LOG);
	    LOGGER.info("Search Command is: " + searchCommand);
	    commandOutput = tapEnv.executeCommandUsingSsh(device, searchCommand);
	    if (CommonMethods.isNotNull(commandOutput)) {
		status = CommonMethods.patternMatcher(commandOutput,
			dcmServerUrl + BroadBandTestConstants.REG_EXPRESSION_DOT_STAR);
		LOGGER.info("Verifying pattern matcher for DCM Server Url: " + status);
	    }
	    errorMessage = "Unable to verify DCM query at startup.";
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : DCM Query is verified successfully.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }	   
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);
    
	    /**
	     * Step 4 : Verify json message is parsed successfully to file rfcresponse.json
	     *
	     */
	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 4: Verify json message is parsed successfully to file rfcresponse.json");
	    LOGGER.info("Expected: The file should not be empty and should display with  json message.");
	    LOGGER.info("***********************************************************");
	    long startTime = System.currentTimeMillis();
	    do {
		tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		if ((hasRfcProps
			? CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_RFC_PARSED_TEXT_PATH)
			: CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_RFC_RESPONSE_LOG))) {
		    searchCommand = (hasRfcProps
			    ? BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
				    BroadBandCommandConstants.FILE_RFC_PARSED_TEXT_PATH)
			    : BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
				    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				    BroadBandCommandConstants.FILE_RFC_RESPONSE_LOG));
		    LOGGER.info("Search Command is: " + searchCommand);
		    commandOutput = tapEnv.executeCommandUsingSsh(device, searchCommand);
		    status = CommonMethods.isNotNull(commandOutput);
		    errorMessage = "Unable to verify json message file in location /tmp/rfcresponse.json.";
		    if (status) {
			tr181ParametersMap = extractWebPaParameterAndValueFromJsonFile(tapEnv, device, commandOutput);
			if(tr181ParametersMap.containsKey(BroadBandTestConstants.RABID_FRAMEWORK_ENABLE))
			  {
				tr181ParametersMap.remove(BroadBandTestConstants.RABID_FRAMEWORK_ENABLE);
			  }
			status = !tr181ParametersMap.isEmpty();
		    }
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
		    && !status);
	    errorMessage = "Unable to verify json message file in location /tmp/rfcresponse.json.";
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Verified presence of rfcresponse.json file in /tmp/ directory.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }	 
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
	
	    /**
	     * Step 5 : Verify list of enabled feature from rfc_configdata.txt
	     *
	     */
	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 5: Verify list of enabled feature from rfc_configdata.txt");
	    LOGGER.info("Expected:The output should display TR-181 parameters");
	    LOGGER.info("***********************************************************");
	    status = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_RFC_CONFIGDATA_LOG);
	    errorMessage = "Unable to verify rfc_configdata.txt file and its parameters.";
	    if (status) {
		status = verifywebPaParameterAndValueInLogFile(tapEnv, device, tr181ParametersMap,
			BroadBandCommandConstants.FILE_RFC_CONFIGDATA_LOG);
		errorMessage = "Failed to verify webpa parameter and its value from rfc_configdata.txt";
	    }
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Verified all Enabled features from rfc_configdata.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }	    
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
	    
	    /**
	     * Step 6 : Verify GET for TR-181 Parameters through WebPA command
	     *
	     */
	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 6: Verify GET for TR-181 Parameters through WebPA command");
	    LOGGER.info("Expected: WebPA should return success message with value for all parameter.");
	    LOGGER.info("***********************************************************");
	    status = verifywebPaParameterAndValueThroughWebPA(tapEnv, device, tr181ParametersMap);
	    errorMessage = "Json ConfigData values are not matching with WebPA Response";
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Verified all webPA parameters through webpa command.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }		   
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);	  
	    
	    /**
	     * Step 7 : Verify DCM response from log file dcmrfc.log
	     *
	     */
	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("***********************************************************");
	    LOGGER.info("STEP 7: Verify DCM response from log file dcmrfc.log");
	    LOGGER.info("Expected: Log message should be present in dcmrfc.log");
	    LOGGER.info("***********************************************************");
	    status = verifywebPaParameterAndValueInDcmRfcLog(tapEnv, device, tr181ParametersMap);
	    errorMessage = "Verification of tr181 parameters aganist dcmrfc.log file failed";
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Verified DCM response in dcmrfclog file.");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }	    
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "Exception Occurred while verifying Enable Disable Features Via DCM after reboot:" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
		    errorMessage, true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-1102");
	LOGGER.info("#######################################################################################");
    }
    
    /**
     * Method to extract webpa parameter and value from Json file
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi} Reference
     * @param device
     *            device
     * @param fileName
     *            fileName used in find command
     *
     * @author Sumathi Gunasekaran 
     * @refactor yamini.s
     * 
     */

    @SuppressWarnings("unchecked")
    public static Map<String, Object> extractWebPaParameterAndValueFromJsonFile(AutomaticsTapApi tapEnv, Dut device,
	    String jsoncommandOutput) {
	LOGGER.debug("Starting Method:extractWebPaParameterAndValueFromJsonFile ");
	int counter = 0;
	ObjectMapper mapper = new ObjectMapper();
	Map<String, Object> tr181ParametersMap = new HashMap<String, Object>();
	try {
	    JSONObject jsonResponse = new JSONObject(jsoncommandOutput);
	    LOGGER.info("Json Array retrieved from rfcresponse.json is: " + jsonResponse);
	    // Extract only the parameters from webPA response
	    JSONArray tr181ParametersJson = (new JSONObject(
		    jsonResponse.getString(BroadBandTestConstants.STRING_FEATURECONTROL)))
			    .getJSONArray(BroadBandTestConstants.STRING_FEATURES);
	    LOGGER.info("TR181 PARAMETERS JSON ARRAY:" + tr181ParametersJson);
	    for (counter = 0; counter < tr181ParametersJson.length(); counter++) {
		String configData = tr181ParametersJson.getJSONObject(counter)
			.getString(BroadBandTestConstants.STRING_CONFIG_DATA);
		LOGGER.info("Config Data: " + configData);
		// Fetching only the config data from Json message and updating the config data and its value in map
		//mapper.readValue() returns object, we need hashmap hence type casting
		tr181ParametersMap.putAll((HashMap<String, Object>) mapper.readValue(configData,
			new TypeReference<Map<String, String>>() {
			}));
		
	    }
	} catch (Exception exception) {
	    LOGGER.error("Exception Occurred while verifying DCM Json Message:" + exception.getMessage());

	}
	LOGGER.debug("Ending Method:extractWebPaParameterAndValueFromJsonFile ");
	return tr181ParametersMap;

    }
    

    /**
     * Method to extract webpa parameter and value from map and verifying it against log file
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi} Reference
     * @param device
     *            device
     * @param fileName
     *            fileName used in find command
     *
     * @author Sumathi Gunasekaran 
     * @refactor yamini.s
     * 
     */
    public static boolean verifywebPaParameterAndValueInLogFile(AutomaticsTapApi tapEnv, Dut device,
	    Map<String, Object> mapValue, String logFile) {
	LOGGER.debug("Starting Method:verifywebPaParameterAndValueInLogFile");
	Map<String, Object> tr181ParametersMap = new HashMap<String, Object>();
	tr181ParametersMap = mapValue;
	boolean status = false;
	LOGGER.info("Fetching values from Map and verfying in log file");
	String catCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_LINUX_CAT,
		BroadBandTestConstants.SINGLE_SPACE_CHARACTER, logFile);
	String response = tapEnv.executeCommandUsingSsh(device, catCommand);
	LOGGER.info("File Contents: \n" + response);
	
       tr181ParametersMap = extractWebPaParameterAndValueFromRfcFile(response);
       status = !tr181ParametersMap.isEmpty();
	
	LOGGER.debug("Ending Method:verifywebPaParameterAndValueInLogFile");
	return status;
    }
    
    /**
     * Method to extract webpa parameter and value from map and verifying it against DCMRFC Log message
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi} Reference
     * @param device
     *            device
     *
     * @author Sumathi Gunasekaran 
     * @refactor yamini.s
     * 
     */
    public static boolean verifywebPaParameterAndValueInDcmRfcLog(AutomaticsTapApi tapEnv, Dut device,
	    Map<String, Object> mapValue) {
	LOGGER.debug("Starting Method:verifywebPaParameterAndValueInDcmRfcLog");
	Map<String, Object> tr181ParametersMap = new HashMap<String, Object>();
	tr181ParametersMap = mapValue;
	String webPAParameter = null;
	String searchCommandForGettingSameValue = null;
	String searchCommandForGettingUpdatedValue = null;
	boolean status = false;
	LOGGER.info("Fetching values from Map and verfying in log file");
	for (String key : tr181ParametersMap.keySet()) {
	    if (!key.equalsIgnoreCase(BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE)) {
	    webPAParameter=key;
	  
	    LOGGER.info("WebPAParameter:" + webPAParameter);
	    searchCommandForGettingSameValue = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
		    BroadBandTestConstants.RFC_PARAM, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,webPAParameter,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,BroadBandTestConstants.RFC_SAME_VALUE_MESSAGE,BroadBandTestConstants.SINGLE_SPACE_CHARACTER,(String) tr181ParametersMap.get(key),
		    BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		    BroadBandCommandConstants.FILE_DCMRFC_LOG);	    
	    LOGGER.info("searchCommandForGettingSameValue is: " + searchCommandForGettingSameValue);
	    searchCommandForGettingUpdatedValue = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
		    BroadBandTestConstants.RFC_UPDATED_PARAM, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,webPAParameter,		    
		    BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
		    BroadBandCommandConstants.FILE_DCMRFC_LOG);	    
	    LOGGER.info("searchCommandForGettingUpdatedValue is: " + searchCommandForGettingUpdatedValue);
	    
		String searchCommandForGettingSetValue = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.RFC_SET_PARAM, webPAParameter,
			BroadBandTestConstants.DOUBLE_QUOTE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandCommandConstants.FILE_DCMRFC_LOG);
		LOGGER.info("searchCommandForGettingSetValue is: " + searchCommandForGettingSetValue);
	    status = CommonUtils.searchLogFiles(tapEnv, device, searchCommandForGettingSameValue) || CommonUtils.searchLogFiles(tapEnv, device, searchCommandForGettingUpdatedValue)||CommonUtils.searchLogFiles(tapEnv, device, searchCommandForGettingSetValue) ;
	    if (!status) {
		return status;
	    }
	    }
	}
	LOGGER.debug("Ending Method:verifywebPaParameterAndValueInDcmRfcLog");
	return status;
    }
    
    /**
     * Method to verify webpa and its response through webpa command
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi} Reference
     * @param device
     *            device
     * @param fileName
     *            fileName used in find command
     *
     * @author Sumathi Gunasekaran
     * @refactor Rakesh C N
     */
    public static boolean verifywebPaParameterAndValueThroughWebPA(AutomaticsTapApi tapEnv, Dut device,
	    Map<String, Object> mapValue) {
	LOGGER.debug("Starting Method:verifywebPaParameterAndValueThroughWebPA");
	Map<String, Object> tr181ParametersMap = new HashMap<String, Object>();
	tr181ParametersMap = mapValue;
	boolean status = false;
	String webPAParameter = null;
	String webPAResponse = null;
	boolean isAccessPointWebpaParameter=false;
	boolean isWifiRadioWebpaParameter=false;
	String matchValue = null;
	String accessPointValue=null;
	LOGGER.info("Fetching values from Map and verfying in log file");
	for (String key : tr181ParametersMap.keySet()) {
	    
	    if (!key.equalsIgnoreCase(BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_ENABLE)) {
		webPAParameter = key;
		
		isAccessPointWebpaParameter = CommonMethods.patternMatcher(webPAParameter,
			BroadBandTestConstants.PATTER_MATCHER_FOR_WIFI_ACCESS_POINT);
		isWifiRadioWebpaParameter = CommonMethods.patternMatcher(webPAParameter,
			BroadBandTestConstants.PATTER_MATCHER_FOR_WIFI_RADIO);
		if (isAccessPointWebpaParameter) {
		    matchValue = CommonMethods.patternFinderToReturnAllMatchedString(webPAParameter,
			    BroadBandTestConstants.PATTER_MATCHER_FOR_WIFI_ACCESS_POINT).get(
			    BroadBandTestConstants.CONSTANT_0);
		    accessPointValue = BroadBandTestConstants.ACCESS_POINT_MAPPING.get(matchValue);
		    webPAParameter = webPAParameter.replaceAll(matchValue, accessPointValue);
		} else if (isWifiRadioWebpaParameter) {
		    matchValue = CommonMethods.patternFinderToReturnAllMatchedString(webPAParameter,
			    BroadBandTestConstants.PATTER_MATCHER_FOR_WIFI_RADIO)
			    .get(BroadBandTestConstants.CONSTANT_0);
		    accessPointValue = BroadBandTestConstants.WIFI_RADIO_MAPPING.get(matchValue);
		    webPAParameter = webPAParameter.replaceAll(matchValue, accessPointValue);
		}
		if (CommonMethods.isNotNull(webPAParameter)) {
		    webPAResponse = tapEnv.executeWebPaCommand(device, webPAParameter).replaceAll("'",
			    BroadBandTestConstants.EMPTY_STRING);
		    LOGGER.info("WebPA Response is:" + webPAResponse);
		    LOGGER.info("The Value to check aganist webPA Response is:" + tr181ParametersMap.get(key));
		    status = webPAResponse.equalsIgnoreCase((String) tr181ParametersMap.get(key));
		    if (!status) {
			return status;
		    }
		}
	    }
	}
	LOGGER.debug("Ending Method:verifywebPaParameterAndValueThroughWebPA");
	return status;
    }
    
    private static Map<String, Object> extractWebPaParameterAndValueFromRfcFile(String fileContents) {
    	Map<String, Object> parameterMap = new HashMap<String, Object>();

    	String patternString = "tr181.(.*)#~(.*)#~";
    	Pattern pattern = Pattern.compile(patternString, Pattern.CASE_INSENSITIVE);
    	Matcher matcher = pattern.matcher(fileContents);
    	int count = matcher.groupCount();
    	LOGGER.info("Group count: " + count);
    	while (matcher.find()) {
    	   
    	    if (matcher.group(BroadBandTestConstants.CONSTANT_1).trim().contains(BroadBandTestConstants.STRING_MAX_ASSOCIATED_DEVICES) && 
    		    BroadBandCommonUtils.convertStringToInteger(matcher.group(BroadBandTestConstants.CONSTANT_2).trim()) >  BroadBandCommonUtils.convertStringToInteger(BroadBandTestConstants.STRING_63)) {
    		parameterMap.put(matcher.group(BroadBandTestConstants.CONSTANT_1).trim(), BroadBandTestConstants.STRING_63);
    	    } else {
    		parameterMap.put(matcher.group(BroadBandTestConstants.CONSTANT_1).trim(), matcher.group(BroadBandTestConstants.CONSTANT_2).trim());
    	    }
    	}

    	for (Map.Entry<String, Object> entry : parameterMap.entrySet()) {
    	    LOGGER.info("Key = " + entry.getKey() + ", Value = " + entry.getValue());
    	}
    	return parameterMap;
        }
    


}